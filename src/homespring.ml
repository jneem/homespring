open List;;
open Str;;

type direction_t    = Upstream | Downstream;;
type age_t          = Young | Mature;;

type salmon_t = {
    mutable name        : string;
    mutable direction   : direction_t;
    mutable age         : age_t;
    mutable ready       : bool; (* If we are ready to move on next tick *)
    mutable ch_number   : int;  (* If we are downstream, this tells us which
                                    child we came from *)
};;

(* remove escape sequences from a token and replace them with the character
 * that they represent *)
let tokenize s =
    let rx1 = regexp " \\." in
    let rx2 = regexp "\\. " in
    let rx3 = regexp "\\([^ ]\\|^\\)\\.\\($\\|[^ ]\\)" in
        global_replace rx1 "."
            (global_replace rx2 " "
                (global_replace rx3 "\\1\\2" s))

(* change a name into a printable name by replacing newline characters with
 * "\n" *)
let print_regexp1 = regexp "\n"
let print_regexp2 = regexp "\t"
let printable s = global_replace print_regexp1 "\\n"
                    (global_replace print_regexp2 "\\t" s)

(* Tests on salmon *)
let mature s = s.age = Mature;;
let young s = s.age = Young;;
let upstream s = s.direction = Upstream;;
let downstream s = s.direction = Downstream;;
let ready s = s.ready;;

class node name (parent : node option) io =
    object (self)
    val mutable water = false
    val mutable power = false
    val mutable snow = false
    val mutable destroyed = false
    val mutable ch_number = 0
    val mutable salmon = ( [] : salmon_t list )
    val mutable born = ( [] : salmon_t list ) (* the salmon that were born in
                                               * this tick *)
    val mutable children = ( [] : node list )
    val mutable finished = false

    method finish =
        match parent with
            None -> finished <- true
          | Some n -> n#finish

    method isFinished = finished

    method reset =
        let x n = n#reset in
        water <- false;
        power <- false;
        snow <- false;
        destroyed <- false;
        salmon <- [];
        born <- [];
        finished <- false;
        iter x children
 
    method setChNum n =
        ch_number <- n;

    method getChNum = ch_number
    method getChildren = children
    method getParent = parent
    method getSalmon = salmon
    method setSalmon s = salmon <- s

    method existsTree f =
        let x n = n#existsTree f in
        exists f salmon || exists x children

    method printTree sp =
        let x n = n#printTree (sp+2) in
        print_string (String.make sp ' ');
        Printf.printf "\"%s\"\n" (printable name);
        iter x children

    method getName = name

    method addChild n =
        n#setChNum (List.length children);
        children <- append children [n];

    method hasName s =
        let x n = n#hasName s in
        if name = s then true else (exists x children)

    method chPowered =
        let p n = n#hasPower in
        exists p children

    method hasPower = power || self#chPowered
    method givesPower = self#hasPower
    method hasWater = water
    method givesWater = water
    method hasSnow  = snow
    method givesSnow = snow
    method hasSalmon = (List.length salmon <> 0)
    method isDestroyed = false

    method waterBlocked = false
    method snowBlocked = false

    method waterTick =
        let w n = n#givesWater in
        let x n = n#waterTick in
        water <- not self#waterBlocked && exists w children;
        iter x children;

    method snowTick =
        let s n = n#givesSnow in
        let x n = n#snowTick in
        snow <- not self#snowBlocked && exists s children;
        if snow then destroyed <- true;
        iter x children;

    method powerTick =
        let x n = n#powerTick in
        iter x children

    method doMiscTick = ()
    method miscTick =
        let x n = n#miscTick in
        self#doMiscTick;
        iter x children

    method inputTick =
        if io#ready then
            self#addSalmon {name = io#get;
                             direction = Upstream;
                             age = Mature;
                             ready = true;
                             ch_number = 0;}

    method spawnSalmon s =
        s.age <- Mature;
        s.direction <- Downstream;
        born <- {name = name;
                 direction = Downstream;
                 age = Young;
                 ready = true;
                 ch_number = 0} :: born;

    method salmonBlocked (s:salmon_t) = false
    method salmonBlockedFrom (n:node) (s:salmon_t) = false
    method salmonVeryBlocked (s:salmon_t) = false

    method addSalmon s =
        salmon <- s :: salmon

    method fishTickUp =
        let x n = n#fishTickUp in
        let processSalmon s =
            match s with
                {direction=d} when d = Downstream -> true
              | {ready=d} when d = false -> s.ready <- true; true
              | {name=n} when n = name -> self#spawnSalmon s; true
              | _ when self#salmonBlocked s -> self#spawnSalmon s; true
              | _ -> try
                    let hasName n = n#hasName s.name in
                    let notBlocked n = not (self#salmonBlockedFrom n s
                                         || n#salmonVeryBlocked s) in
                    let good n = notBlocked n && hasName n in
                    let best = List.find good children in
                        best#addSalmon s; false
                    with Not_found ->
                        try
                            let notBlocked n = not (n#salmonVeryBlocked s) in
                            (List.find notBlocked children)#addSalmon s; false
                        with Not_found -> self#spawnSalmon s; true
        in
            iter x children;
            salmon <- filter processSalmon salmon;

    method fishTickDown =
        let x n = n#fishTickDown in
        let processSalmon s =
            match s with
                {direction=d} when d = Upstream -> true
              | {ready=d} when d = false -> s.ready <- true; true
              | _ when self#salmonBlocked s -> true
              | _ -> match parent with
                        None -> io#print s.name; false
                      | Some p ->   if p#salmonVeryBlocked s then
                                        true
                                    else
                                        (s.ch_number <- ch_number;
                                         p#addSalmon s; false)
        in
            salmon <- filter processSalmon salmon;
            iter x children;

    method fishTickHatch = let x n = n#fishTickHatch in iter x children

    method fishTickEnd =
        let x n = n#fishTickEnd in
        salmon <- append born salmon;
        born <- [];
        iter x children

    method fishTick =
        self#fishTickDown;
        self#fishTickUp;
        self#fishTickHatch;
        self#fishTickEnd;

end
and virtual destroyable name parent io =
    object(self) inherit node name parent io
    method isDestroyed = destroyed

end
and spring name parent io =
    object(self) inherit node name parent io
    method waterTick =
        let x n = n#waterTick in
        water <- true;
        iter x children

end
and bear name parent io =
    object(self) inherit node name parent io
    method doMiscTick = salmon <- filter young salmon;

end
and hatchery name parent io =
    object (self) inherit destroyable name parent io
    method fishTickHatch =
        let x n = n#fishTickHatch in
        if self#hasPower && not self#isDestroyed then
            born <- {name = "homeless";
                     direction = Upstream;
                     age = Young;
                     ready = true;
                     ch_number = 0} :: born;
        iter x children

end
and hydro_power name parent io =
    object(self) inherit destroyable name parent io
    method powerTick =
        let x n = n#powerTick in
            power <- self#hasWater && (not self#isDestroyed);
            iter x children

end
and snowmelt name parent io =
    object(self) inherit node name parent io
    method snowTick =
        let x n = n#snowTick in
        snow <- true;
        iter x children

end
and shallows name parent io =
    object(self) inherit node name parent io
    method addSalmon s =
        salmon <- s :: salmon;
        if s.age = Mature then
            s.ready <- false

end
and append_down name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let str = ref "" in
        let addName s = str := !str ^ s.name in
        let appendCh s = (downstream s) && s.ch_number <> 0 in
        let doAppend s = if (downstream s) then s.name <- s.name ^ !str in
        let (throw,keep) = partition appendCh salmon in
            iter addName throw;
            iter doAppend keep;
            salmon <- keep;

end
and force_field name parent io =
    object(self) inherit node name parent io
    method waterBlocked = self#hasPower
    method snowBlocked = self#hasPower
    method salmonBlocked s = self#hasPower

end
and sense name parent io =
    object(self) inherit node name parent io
    method hasPower = not (exists mature salmon) && self#chPowered

end
and clone name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let clone s = {name=s.name; age=Young; direction=Downstream;
                       ch_number=0; ready=true;} in
        salmon <- append salmon (map clone salmon)

end
and young_bear name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let (y,m) = partition young salmon in
        let keep = ref false in
        let keep_half s = keep := not !keep; !keep in
        salmon <- append y (filter keep_half m)

end
and bird name parent io =
    object(self) inherit node name parent io
    method doMiscTick = salmon <- (filter mature salmon)

end
and upstream_killing_device name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        if self#hasPower && length children > 1 then begin
            (nth children (length children - 1))#setSalmon []
        end

end
and waterfall name parent io =
    object(self) inherit node name parent io
    method salmonBlocked s = upstream s

end
and universe name parent io =
    object(self) inherit destroyable name parent io
    method doMiscTick =
        if destroyed then
            self#finish

end
and rapids name parent io =
    object(self) inherit node name parent io
    method addSalmon s =
        salmon <- s :: salmon;
        if s.age = Young then
            s.ready <- false

end
and powers name parent io =
    object (self) inherit node name parent io
    method powerTick =
        let x n = n#powerTick in
        power <- true;
        iter x children

end
and marshy name parent io =
    object(self) inherit node name parent io as super
    val mutable give_snow = false
    method snowTick =
        give_snow <- snow;
        super#snowTick
    method givesSnow = give_snow
    method hasSnow = give_snow || snow
    method reset = give_snow <- false; super#reset

end
and insulated name parent io =
    object(self) inherit node name parent io
    method hasPower = false

end
and upstream_sense name parent io =
    object(self) inherit node name parent io
    method hasPower =
        let bad s = (mature s) && (upstream s) in
        not (exists bad salmon) && self#chPowered

end
and downstream_sense name parent io =
    object(self) inherit node name parent io
    method hasPower =
        let bad s = (mature s) && (downstream s) in
        not (exists bad salmon) && self#chPowered

end
and evaporates name parent io =
    object(self) inherit node name parent io
    method snowBlocked = self#hasPower
    method waterBlocked = self#hasPower

end
and youth_fountain name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        iter (fun s -> s.age <- Young) salmon

end
and oblivion name parent io =
    object(self) inherit destroyable name parent io
    method doMiscTick =
        let obliviate s = s.name <- "" in
        if self#hasPower && not destroyed then
            iter obliviate salmon

end
and pump name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = not self#hasPower

end
and range_sense name parent io =
    object(self) inherit node name parent io as super
    method hasPower = super#hasPower && not (self#existsTree mature)

end
and fear name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = self#hasPower

end
and reverse_up name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let processSalmon n s =
            if (downstream s) && s.ch_number = 1
                              && not (n#salmonVeryBlocked
                                        {s with direction = Upstream}) then
                (s.direction <- Upstream; n#addSalmon s; false)
            else true
        in
        if length children >= 2 then
            salmon <- filter (processSalmon (hd children)) salmon

end
and reverse_down name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let processSalmon n s =
            if (downstream s) && s.ch_number = 0
                              && not (n#salmonVeryBlocked
                                        {s with direction = Upstream}) then
                (s.direction <- Upstream; n#addSalmon s; false)
            else true
        in
        if length children >= 2 then
            salmon <- filter (processSalmon (nth children 1)) salmon

end
and time name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let oldify s = s.age <- Mature in
        iter oldify salmon

end
and lock name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = (downstream s) && self#hasPower
    method snowBlocked = self#hasPower

end
and inverse_lock name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = (downstream s) && not self#hasPower
    method snowBlocked = not self#hasPower

end
and young_sense name parent io =
    object(self) inherit node name parent io
    method hasPower = not (exists young salmon) && self#chPowered

end
and switch name parent io =
    object(self) inherit node name parent io
    method hasPower = exists mature salmon && self#chPowered

end
and young_switch name parent io =
    object(self) inherit node name parent io
    method hasPower = exists young salmon && self#chPowered

end
and narrows name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = (length salmon) <> 0

end
and append_up name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let str = ref "" in
        let addName s = str := !str ^ s.name in
        let appendCh s = (downstream s) && s.ch_number <> 0 in
        let doAppend s = if (upstream s) then s.name <- s.name ^ !str in
        let (throw,keep) = partition appendCh salmon in
            iter addName throw;
            iter doAppend keep;
            salmon <- keep;

end
and young_range_sense name parent io =
    object(self) inherit node name parent io
    method hasPower = not (self#existsTree young) && self#chPowered

end
and net name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = mature s

end
and force_down name parent io =
    object(self) inherit reverse_down name parent io
    method salmonBlockedFrom n s =
        (upstream s) && n#getChNum = (length children)-1

end
and force_up name parent io =
    object(self) inherit reverse_up name parent io
    method salmonBlockedFrom n s = (upstream s) && n#getChNum = 0

end
and spawn name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        if self#hasPower then begin
            let spawn n s = n#spawnSalmon s in
            let spawnOnce n = iter (spawn n) n#getSalmon in
            let rec spawnAll n = spawnOnce n; iter spawnAll n#getChildren in
            let spawnMe s = self#spawnSalmon s in
            iter spawnMe salmon;
            iter spawnAll children;
            self#fishTickEnd
        end

end
and power_invert name parent io =
    object(self) inherit node name parent io as super
    method hasPower = if destroyed then super#hasPower else not super#hasPower

end
and current name parent io =
    object(self) inherit node name parent io
    method salmonVeryBlocked s = young s

end
and bridge name parent io =
    object(self) inherit destroyable name parent io
    method salmonVeryBlocked s = destroyed
    method waterBlocked = destroyed
    method snowBlocked = destroyed

end
and split name parent io =
    object(self) inherit node name parent io
    method doMiscTick =
        let splitSalmon s =
            let split c = born <- append born [{name = (String.make 1 c);
                                               direction = s.direction;
                                               age = s.age;
                                               ch_number = s.ch_number;
                                               ready = true;}]
            in
            String.iter split s.name;
        in
        iter splitSalmon salmon;
        salmon <- born;
        born <- []

end
and range_switch name parent io =
    object(self) inherit node name parent io
    method hasPower = self#existsTree mature && self#chPowered

end
and young_range_switch name parent io =
    object(self) inherit node name parent io
    method hasPower = self#existsTree young && self#chPowered
end

let make_node name =
    match (String.lowercase name) with
        "hatchery" -> new hatchery name
      | "hydro power" -> new hydro_power name
      | "snowmelt" -> new snowmelt name
      | "shallows" -> new shallows name
      | "rapids" -> new rapids name
      | "append down" -> new append_down name
      | "bear" -> new bear name
      | "force field" -> new force_field name
      | "sense" -> new sense name
      | "clone" -> new clone name
      | "young bear" -> new young_bear name
      | "bird" -> new bird name
      | "upstream killing device" -> new upstream_killing_device name
      | "waterfall" -> new waterfall name
      | "universe" -> new universe name
      | "powers" -> new powers name
      | "marshy" -> new marshy name
      | "insulated" -> new insulated name
      | "upstream sense" -> new upstream_sense name
      | "downstream sense" -> new downstream_sense name
      | "evaporates" -> new evaporates name
      | "youth fountain" -> new youth_fountain name
      | "oblivion" -> new oblivion name
      | "pump" -> new pump name
      | "range sense" -> new range_sense name
      | "fear" -> new fear name
      | "reverse up" -> new reverse_up name
      | "reverse down" -> new reverse_down name
      | "time" -> new time name
      | "lock" -> new lock name
      | "inverse lock" -> new inverse_lock name
      | "young sense" -> new young_sense name
      | "switch" -> new switch name
      | "young switch" -> new young_switch name
      | "narrows" -> new narrows name
      | "append up" -> new append_up name
      | "young range sense" -> new young_range_sense name
      | "net" -> new net name
      | "force down" -> new force_down name
      | "force up" -> new force_up name
      | "spawn" -> new spawn name
      | "power invert" -> new power_invert name
      | "current" -> new current name
      | "bridge" -> new bridge name
      | "split" -> new split name
      | "range switch" -> new range_switch name
      | "young range switch" -> new young_range_switch name
      | _ -> new spring name

(* return a list of all the tokens in the file *)
let parse_file filename =
    let ch = open_in filename in
    let s = String.make (in_channel_length ch) ' ' in
    really_input ch s 0 (in_channel_length ch);
    let r = Str.regexp "\\(\\. \\| \\.\\|[^ \n\\.]\\)*\\.?[ \n]" in
    let newline = Str.regexp ".*\\.\n" in
    let read_token pos =
        if (Str.string_match r s pos) then
            let str = Str.matched_string s in
            let fin = Str.match_end () in
            if (Str.string_match newline str 0) then (str,fin)
            else ( (Str.string_before str ((String.length str)-1)), fin )
        else ("", (pos+1))
    in
    let rec read_all pos =
        let (tk, p) = read_token pos in
            try
                tk :: read_all p
            with Invalid_argument "Str.string_match" -> []
    in
        read_all 0

(* Turn a string of tokens into a tree *)
let rec build_tree_rec level toks parent io =
    match !toks with
        [] -> None
      | x::xs when x = "" && (level > 1) -> toks := tl !toks; None
      | _ ->
            let n = make_node (tokenize (hd !toks)) parent io in
            toks := tl !toks;
            add_children (level + 1) n toks io;
            Some n
and add_children level n toks io =
    let child = build_tree_rec level toks (Some n) io in
        match child with
            Some ch -> n#addChild ch; add_children level n toks io
          | _ -> ()
;;

let build_tree filename io = 
    build_tree_rec 0 (ref (parse_file filename)) None io
;;

