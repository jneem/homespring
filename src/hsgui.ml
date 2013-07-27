(* vim: set et : *)

open Homespring;;
open Hscanvas;;
open GnoCanvas;;
open Gtk;;
open GAction;;
open Printf;;

let delete ev = GMain.Main.quit (); false in

let ui_desc =
"<ui>\
   <menubar name='MenuBar'>\
       <menu action='FileMenu'>\
           <menuitem action='Open' />\
           <menuitem action='Close' />\
           <menuitem action='Quit' />\
       </menu>\
       <menu action='ViewMenu'>\
           <menuitem action='Zoom in' />\
           <menuitem action='Zoom out' />\
           <menuitem action='Zoom fit' />\
           <menuitem action='Zoom 100' />\
       </menu>\
       <menu action='ActionMenu'>\
           <menuitem action='Run' />\
           <menuitem action='Pause' />\
           <menuitem action='Step' />\
           <menuitem action='Tick' />\
           <menuitem action='Reset' />\
       </menu>\
   </menubar>\
   <toolbar name='ToolBar'>\
       <toolitem action='Open' />\
       <toolitem action='Close' />\
       <separator />\
       <toolitem action='Zoom in' />\
       <toolitem action='Zoom out' />\
       <toolitem action='Zoom fit' />\
       <toolitem action='Zoom 100' />\
       <separator />\
       <toolitem action='Run' />\
       <toolitem action='Pause' />\
       <toolitem action='Step' />\
       <toolitem action='Tick' />\
       <toolitem action='Reset' />\
   </toolbar>\
</ui>"
in

(* the list of (homespring canvas, text_view) *)
let hsroot = ref [] in

(* Set up the toplevel widgets *)
let notebook = GPack.notebook () in
let win = GWindow.window () in
let vbox = GPack.vbox () in
let ui = ui_manager () in
let _ = win#event#connect#delete ~callback:delete in
win#set_default_size 800 600;
win#add vbox#coerce;

let page_action f =
    if notebook#current_page >= 0 then
        f (List.nth !hsroot notebook#current_page)
in

let page_test f =
    if notebook#current_page >= 0 then
        f (List.nth !hsroot notebook#current_page)
    else
        false
in

(* widget sensitivity *)
let update_sense () = 
    let blank_all = ((List.length !hsroot) = 0) in
    let blank_step = not (page_test (fun n -> not n#isFinished)) in
    let show_s = not (blank_step || blank_all) in
    let show_a = not blank_all in
    let show_r = show_s && page_test (fun n -> not n#isRunning) in
    let show_p = show_s && page_test (fun n -> n#isRunning) in

    (ui#get_widget "/MenuBar/ViewMenu/Zoom in")#misc#set_sensitive show_a;
    (ui#get_widget "/MenuBar/ViewMenu/Zoom out")#misc#set_sensitive show_a;
    (ui#get_widget "/MenuBar/ViewMenu/Zoom fit")#misc#set_sensitive show_a;
    (ui#get_widget "/MenuBar/ViewMenu/Zoom 100")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Zoom in")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Zoom out")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Zoom fit")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Zoom 100")#misc#set_sensitive show_a;

    (ui#get_widget "/MenuBar/ActionMenu/Run")#misc#set_sensitive show_r;
    (ui#get_widget "/MenuBar/ActionMenu/Pause")#misc#set_sensitive show_p;
    (ui#get_widget "/MenuBar/ActionMenu/Step")#misc#set_sensitive show_s;
    (ui#get_widget "/MenuBar/ActionMenu/Tick")#misc#set_sensitive show_s;
    (ui#get_widget "/MenuBar/ActionMenu/Reset")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Run")#misc#set_sensitive show_r;
    (ui#get_widget "/ToolBar/Pause")#misc#set_sensitive show_p;
    (ui#get_widget "/ToolBar/Step")#misc#set_sensitive show_s;
    (ui#get_widget "/ToolBar/Tick")#misc#set_sensitive show_s;
    (ui#get_widget "/ToolBar/Reset")#misc#set_sensitive show_a;

    (ui#get_widget "/MenuBar/FileMenu/Close")#misc#set_sensitive show_a;
    (ui#get_widget "/ToolBar/Close")#misc#set_sensitive show_a;
in

(* open a homespring file and add it to the notebook as a new tab *)
let open_hs name =
    let c = new Hscanvas.canvas name in
    hsroot := List.append !hsroot [c];
    notebook#append_page
        ~tab_label:(GMisc.label
            ~text:(Filename.basename name) ())#coerce
        c#getPane#coerce;
in

let open_file action = 
    let d = GWindow.file_chooser_dialog ~title:"Open File" ~action:`OPEN () in
    d#add_button_stock `CANCEL `CANCEL;
    d#add_select_button_stock `OPEN `ACCEPT;
    d#set_select_multiple true;
    if d#run () = `ACCEPT then
        List.iter open_hs d#get_filenames;
    d#destroy ();
    update_sense ();
in

let close_file action =
    page_action (fun n -> n#pause);
    let n = notebook#current_page in
    if n >= 0 then
        (* there must be an easier way to erase the nth element of a list *)
        (hsroot := List.filter (fun a -> a != (List.nth !hsroot n)) !hsroot;
        notebook#remove_page n);
    update_sense ();
in

let quit action = GMain.Main.quit() in

let zoom_in ac = page_action (fun n -> n#zoom_in) in
let zoom_out ac = page_action (fun n -> n#zoom_out) in
let zoom_100 ac = page_action (fun n -> n#zoom_100) in
let zoom_fit ac = page_action (fun n -> n#zoom_fit) in
let step ac = page_action (fun c -> c#step); update_sense () in
let tick ac = page_action (fun c -> c#tick); update_sense () in

let run_cb canvas () =
    canvas#step;
    if canvas#isFinished then
        (canvas#pause; update_sense (); false)
    else
        (update_sense (); true)
in
let run ac =
    page_action (fun c ->
                    if not c#isRunning then
                        c#run (Glib.Timeout.add 500 (run_cb c)));
    update_sense ();
in
let pause ac = page_action (fun c -> c#pause); update_sense () in
let reset ac = page_action (fun c -> c#reset); update_sense () in

(* Set up all the actions *)
let ac_grp = GAction.action_group ~name:"MenuActions" () in
add_action "FileMenu" ~label:"_File" ac_grp;
add_action "ViewMenu" ~label:"_View" ac_grp;
add_action "ActionMenu" ~label:"_Action" ac_grp;
add_action "Open" ~stock:`OPEN ~callback:open_file ac_grp;
add_action "Close" ~stock:`CLOSE ~callback:close_file ac_grp;
add_action "Quit" ~stock:`QUIT ~callback:quit ac_grp;
add_action "Zoom in" ~stock:`ZOOM_IN ~callback:zoom_in ac_grp;
add_action "Zoom out" ~stock:`ZOOM_OUT ~callback:zoom_out ac_grp;
add_action "Zoom fit" ~stock:`ZOOM_FIT ~callback:zoom_fit ac_grp;
add_action "Zoom 100" ~stock:`ZOOM_100 ~callback:zoom_100 ac_grp;
add_action "Run" ~stock:`MEDIA_PLAY ~label:"_Run" ~accel:"<control>R"
    ~tooltip:"Step every 1/2 second" ~callback:run ac_grp;
add_action "Pause" ~stock:`MEDIA_PAUSE ~label:"_Pause" ~accel:"F3"
    ~tooltip:"Stop running" ~callback:pause ac_grp;
add_action "Step" ~stock:`MEDIA_FORWARD ~label:"_Step" ~accel:"<control>S"
    ~tooltip:"Run a single step" ~callback:step ac_grp;
add_action "Tick" ~stock:`MEDIA_NEXT ~label:"_Tick" ~accel:"<control>T"
    ~tooltip:"Run a single tick" ~callback:tick ac_grp;
add_action "Reset" ~stock:`REFRESH ~label:"_Reset" ~accel:"<control>N"
    ~tooltip:"Reset the river" ~callback:reset ac_grp;

ui#insert_action_group ac_grp 1;
ignore (ui#add_ui_from_string ui_desc);
win#add_accel_group ui#get_accel_group;

let menus = ui#get_widget "/MenuBar" in
let tool = ui#get_widget "/ToolBar" in
vbox#pack ~expand:false menus;
vbox#pack ~expand:false tool;
vbox#pack ~expand:true ~fill:true notebook#coerce;
ignore (notebook#connect#after#switch_page
    ~callback:(fun a -> update_sense ()));

win#show ();

if Array.length Sys.argv > 1 then
    Array.iter open_hs (Array.sub Sys.argv 1 ((Array.length Sys.argv) - 1));

update_sense ();

GMain.Main.main ();
