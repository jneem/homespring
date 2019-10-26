open Gobject;;
open Gtk;;
open Homespring;;
open Printf;;
open List;;

type homespring_state = SnowTick | WaterTick | PowerTick | FishTick | MiscTick
                      | InputTick

let choose (t,f) b = if b then t else f
let render w h s = Rsvg.render_from_file ~size_cb:(fun x y -> (w, h)) s

(* I/O to the textview *)
class io (buf : GText.buffer) =
    object(self)
    val mutable out_flag = false (* prevent us from re-inputting our output *)
    val mutable curline = ""
    val mutable lines = []

    method set_unedit =
        buf#remove_tag_by_name "fixed" ~start:buf#start_iter ~stop:buf#end_iter;
        buf#apply_tag_by_name "fixed" ~start:buf#start_iter ~stop:buf#end_iter

    method insert_cb s =
        if not out_flag then
            if String.get s ((String.length s) - 1) = '\n' then
                (lines <- lines @ [curline]; curline <- ""; self#set_unedit)
            else
                curline <- curline ^ s

    method print s =
        out_flag <- true;
        buf#insert ~iter:buf#end_iter s;
        out_flag <- false;
        self#set_unedit

    method ready = List.length lines > 0

    method get =
        let ret = hd lines in
        lines <- (tl lines);
        ret
end;;

let insert_cb io iter s = io#insert_cb s

(* we supply an empty pic with which to initialise the nodes; this makes it go
much faster *)
let init_pic = GdkPixbuf.create ~width:1 ~height:1 ()

class gui_node group (canvas : GnoCanvas.canvas) (hs_node : node) =
    let f_l = GnoCanvas.group canvas#root in

    object(self)
    method getCanvas = canvas
    val mutable children = []
    val mutable lines = []

    val mutable s_pic = (init_pic, init_pic)
    val mutable w_pic = (init_pic, init_pic)
    val mutable p_pic = (init_pic, init_pic)
    val mutable f_pic = (init_pic, init_pic)

    val bg = GnoCanvas.rect group
    val text = GnoCanvas.text ~text:(printable hs_node#getName)
                              ~anchor:`NW group
    val snow = GnoCanvas.pixbuf group
    val water = GnoCanvas.pixbuf group
    val power = GnoCanvas.pixbuf group
    val fish = GnoCanvas.pixbuf group

    val fish_list = f_l
    val fish_list_bg = GnoCanvas.rect f_l
    val fish_list_text = GnoCanvas.text f_l
    val mutable fish_hidden = true

    (* these are the width and height of the trees rooted at this node *)
    val mutable height = 0.0
    val mutable width = 0.0

    val mutable self_width = 0.0
    val mutable self_height = 0.0

    method printTree spaces =
        let x n = n#printTree (spaces + 2) in
        print_string (String.make spaces ' ');
        printf "\"%s\"\n" (printable hs_node#getName);
        List.iter x children

    method hs_node = hs_node
    method getHeight = height
    method getWidth = width
    method getSelfHeight = self_height
    method getSelfWidth = self_width

    method reset_pixbufs =
        snow#set [`PIXBUF (choose s_pic hs_node#hasSnow)];
        water#set [`PIXBUF (choose w_pic hs_node#hasWater)];
        power#set [`PIXBUF (choose p_pic hs_node#hasPower)];
        fish#set [`PIXBUF (choose f_pic hs_node#hasSalmon)];

    (* build a tree of gui_nodes rooted at this node *)
    method init_tree =
        let make_child n =
            (new gui_node (GnoCanvas.group canvas#root) canvas n,
             GnoCanvas.line canvas#root)
        in
        let x n = n#init_tree in
        let (a,b) = List.split (List.map make_child hs_node#getChildren) in
        children <- a; lines <- b;
        List.iter x children;

    (* build a tree of gui_nodes rooted at this node and lay them out. Return
       (minx, miny, maxx, maxy) *)
    method init =
        self#reset_pixbufs;
        canvas#set_pixels_per_unit 1.0;
        canvas#set_center_scroll_region true;
        self#init_tree;
        self#resize 1.0;

    (* re-layout the tree and calculate the canvas size *)
    method resize zoom =
        let size = zoom *. 16.0 in
        let point_size = zoom *. 12.0 in
        let pic = int_of_float size in
        let datadir = 
                try Sys.getenv "DATADIR" with Not_found -> 
                        (if (Sys.file_exists("snow_on.svg")) then "."
                         else if (Sys.file_exists("data/snow_on.svg")) then "data"
                         else raise (Failure "data dir not found (set DATADIR env to directory containing e.g. snow_on.svg)"))
        in

        s_pic <- ( (render pic pic (datadir^"/snow_on.svg")),
                   (render pic pic (datadir^"/snow_off.svg")));
        w_pic <- ( (render pic pic (datadir^"/water_on.svg")),
                   (render pic pic (datadir^"/water_off.svg")));
        p_pic <- ( (render pic pic (datadir^"/power_on.svg")),
                   (render pic pic (datadir^"/power_off.svg")));
        f_pic <- ( (render pic pic (datadir^"/fish_on.svg")),
                   (render pic pic (datadir^"/fish_off.svg")));
        self#calc_size ~point_size ~padding:size ~pic_size:size;
        self#layout 0.0 0.0 ~size ~s:s_pic ~w:w_pic ~p:p_pic ~f:f_pic;
        canvas#set_scroll_region
            (-.size)
            0.0
            self#getWidth
            (self#getHeight +. (size *. 2.0))

    (* calculate the size of this node and the size of this subtree *)
    method calc_size ~point_size ~padding ~pic_size =
        let x n = n#calc_size ~point_size ~padding ~pic_size in
        let sum_h h node = node#getHeight +. h +. padding in
        let max_w w node = max node#getWidth w in
        text#set [`SIZE_POINTS point_size];
        fish_list_text#set [`SIZE_POINTS (point_size *. 0.75)];
        self_width <- max text#text_width (pic_size *. 4.0);
        self_height <- pic_size +. text#text_height;

        iter x children;
        height <- max ((fold_left sum_h 0.0 children) -. padding) self_height;
        width <- (fold_left max_w 0.0 children) +. self_width +. padding;

    (* position every node in this subtree. *)
    method layout x y ~size ~s ~w ~p ~f =
        let child_y = ref y in
        let child_x = x +. self_width +. size in
        let self_y = (y +. (height *. 0.5)) in
        let rec lay_children = function
            (ch::xs, line::ys) ->
                    ch#layout child_x !child_y ~size ~s ~w ~p ~f;
                    line#set [`POINTS 
                                    [| (x +. self_width *. 0.5);
                                       (self_y +. self_height *. 0.5);
                                       (child_x +. ch#getSelfWidth *. 0.5);
                                       (!child_y +. ch#getSelfHeight *. 0.5
                                                 +. ch#getHeight *. 0.5) |];
                              `FILL_COLOR "black"];
                    line#lower_to_bottom ();
                    lines <- line :: lines;
                    child_y := !child_y +. ch#getHeight +. size;
                    lay_children (xs,ys)
          | _ -> 0.0
        in
        let border = size *. 0.2 in
        s_pic <- s; w_pic <- w; p_pic <- p; f_pic <- f;
        self#reset_pixbufs;
        group#set   [`X x;              `Y self_y];
        snow#set    [`X 0.0;            `Y 0.0];
        water#set   [`X size;           `Y 0.0];
        power#set   [`X (size *. 2.0);  `Y 0.0];
        fish#set    [`X (size *. 3.0);  `Y 0.0];
        text#set    [`X 0.0;            `Y size];
        bg#set      [`X1 (-.border); `X2 (self_width +. border);
                     `Y1 (-.border); `Y2 (self_height +. border);
                     `FILL_COLOR "white"; `OUTLINE_COLOR "black"];
        fish_list#set [`X x; `Y (self_y +. self_height)];
        ignore (lay_children (children, lines));
        fish_list#raise_to_top ();

    method update_fish_list =
        let f_str str sal = str ^
            (if (mature sal) then "M\t" else "Y\t") ^
            (if (upstream sal) then "U\t" else "D\t") ^
            (printable sal.name) ^"\n"
        in
        let t = List.fold_left f_str "" hs_node#getSalmon in
        if String.length t > 0 then
            fish_list_text#set [`TEXT (String.sub t 0 ((String.length t) - 1))]
        else
            fish_list_text#set [`TEXT ""];
        fish_list_bg#set [`X1 (-.2.0); `Y1 (-.2.0);
                          `X2 (fish_list_text#text_width +. 2.0);
                          `Y2 (fish_list_text#text_height +. 2.0)];

    method toggle_fish_list =
        if fish_hidden then
            (fish_hidden <- false; fish_list#show ())
        else
            (fish_hidden <- true; fish_list#hide ());
        self#update_fish_list;

    method update =
        let x n = n#update in
        if not fish_hidden then
            self#update_fish_list;
        self#reset_pixbufs;
        iter x children;

    initializer
        fish_list_bg#set [`X1 0.0; `Y1 0.0;
                          `FILL_COLOR_RGBA (Int32.of_string "0xFFFFFFAA");
                          `OUTLINE_COLOR_RGBA (Int32.of_string "0x000000FF")];
        fish_list_text#set [`X 0.0; `Y 0.0; `ANCHOR `NW];
        ignore (group#connect#event
            ~callback:(function
                `BUTTON_PRESS b ->
                    if (GdkEvent.Button.button b) = 1 then
                        self#toggle_fish_list; false
                | _ -> false))
end;;

class canvas (file : string) =
    let c = GnoCanvas.canvas () in
    let t = GText.view () in
    let io = (new io t#buffer) in
    let r = match build_tree file io with
            None -> new Homespring.node
                        "In Homespring, the null program is not a quine.\n"
                        None io
            | Some n -> n
    in
    let scroll1_v = GData.adjustment ~step_incr:25.0 () in
    let scroll1_h = GData.adjustment ~step_incr:25.0 () in

    object(self)
    val mutable state = SnowTick
    val mutable run_id = None
    val gui_root = new gui_node (GnoCanvas.group c#root) c r
    val root = r
    val scroll1 = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC
        ~vadjustment:scroll1_v ~hadjustment:scroll1_h ()
    val scroll2 = GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC()
    val pane = GPack.paned `VERTICAL ()
    val text = t
    val canvas = c

    val mutable zoom = 1.0
    val mutable min_zoom = 0.15
    val mutable max_zoom = 4.0

    method getPane = pane
    method getRoot = gui_root
    method root = gui_root#hs_node
    method isFinished = root#isFinished
    method tick =
        if not root#isFinished then begin
            (match state with
                SnowTick -> root#snowTick; state <- WaterTick
              | WaterTick -> root#waterTick; state <- PowerTick
              | PowerTick -> root#powerTick; state <- FishTick
              | FishTick -> root#fishTick; state <- MiscTick
              | MiscTick -> root#miscTick; state <- InputTick
              | InputTick -> root#inputTick; state <- SnowTick);
            gui_root#update;
            flush stdout
        end
    method step =
        match state with
            InputTick -> self#tick
          | _ -> self#tick; self#step

    method run id = run_id <- Some id
    method pause = (match run_id with
        Some id -> Glib.Timeout.remove id
      | None -> ());
        run_id <- None;
    method isRunning = match run_id with Some i -> true | None -> false
    method isFinished = root#isFinished
    method reset =
        root#reset;
        gui_root#update;
        run_id <- None;
        state <- SnowTick

    method resize =
        let ratio_x = scroll1_h#value /. scroll1_h#upper in
        let ratio_y = scroll1_v#value /. scroll1_v#upper in
        gui_root#resize zoom;
        scroll1_h#set_value (min
            (ratio_x *. scroll1_h#upper)
            (scroll1_h#upper -. scroll1_h#page_size));
        scroll1_v#set_value (min
            (ratio_y *. scroll1_v#upper)
            (scroll1_v#upper -. scroll1_v#page_size));
        flush stdout

    method zoom_in =
        zoom <- min (zoom *. 1.2) max_zoom;
        self#resize

    method zoom_out =
        zoom <- max (zoom *. (5.0 /. 6.0)) min_zoom;
        self#resize

    method zoom_100 =
        zoom <- 1.0;
        self#resize

    method zoom_fit =
        let r = scroll1#misc#allocation in
        let w = float_of_int r.width in
        let h = float_of_int r.height in
        let z = min (w /. gui_root#getWidth) (h /. gui_root#getHeight) in
        zoom <- min (max (z *. zoom *. 0.9) min_zoom) max_zoom;
        self#resize

    initializer
        gui_root#getCanvas#set_pixels_per_unit 1.0;
        gui_root#getCanvas#set_center_scroll_region true;
        gui_root#init;
        scroll1#add gui_root#getCanvas#coerce;
        scroll2#add text#coerce;
        pane#pack1 ~resize:true ~shrink:true scroll1#coerce;
        pane#pack2 ~shrink:true scroll2#coerce;

        ignore (canvas#event#connect#scroll ~callback:(fun s ->
            let test x = Gdk.Convert.test_modifier x (GdkEvent.Scroll.state s)
            in
            let dir = GdkEvent.Scroll.direction s in
            let d = if dir = `UP || dir = `LEFT then (-.1.0) else 1.0 in
            if test `SHIFT then
                scroll1_h#set_value (min
                    (scroll1_h#value +. scroll1_h#step_increment *. d)
                    (scroll1_h#upper -. scroll1_h#page_size))
            else if test `CONTROL then
                if d < 0.0 then self#zoom_in else self#zoom_out
            else
                scroll1_v#set_value (min
                    (scroll1_v#value +. scroll1_v#step_increment *. d)
                    (scroll1_v#upper -. scroll1_v#page_size));
            true));

        (*ignore (scroll1_v#connect#value_changed ~callback:(fun s ->
            Printf.printf "value %f, upper %f, page-size %f\n"
                scroll1_v#value scroll1_v#upper scroll1_v#page_size;
            flush stdout));*)

        let _ = t#buffer#create_tag ~name:"fixed"
                                    [`EDITABLE false; `EDITABLE_SET true]
        in ();
        let _ = t#buffer#connect#insert_text
            ~callback:(insert_cb io) in ();
end;;

