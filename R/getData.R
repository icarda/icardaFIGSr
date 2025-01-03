.icardaFIGSEnv <- new.env(parent = emptyenv())

# GUI for getting username and password from user

#' @keywords internal
.authenticate <- function(){
  tt <- tcltk::tktoplevel()
  tcltk::tkwm.title(tt, "Login")
  
  ss <- "Please enter username and password"
  tcltk::tkgrid(tcltk::tklabel(tt, text = ss), columnspan = 2, padx = 50, pady = 10)
  
  usr <- tcltk::tclVar("")
  pwd <- tcltk::tclVar("")
  
  usr_label <- tcltk::tklabel(tt, text = "Username:")
  pwd_label <- tcltk::tklabel(tt, text = "Password:")
  
  usr_input <- tcltk::tkentry(tt, width = "30", textvariable = usr)
  pwd_input <- tcltk::tkentry(tt, width = "30", textvariable = pwd, show = "*")
  
  tcltk::tkgrid(usr_label, usr_input, sticky = "ew", padx = 5)
  tcltk::tkgrid(pwd_label, pwd_input, sticky = "ew", padx = 5)
  
  on_okay <- function() {
    .credentials <- list("username" = tcltk::tclvalue(usr), "password" = tcltk::tclvalue(pwd))
    assign(".credentials", .credentials, envir = .icardaFIGSEnv)
    tcltk::tkdestroy(tt)
  }
  
  ok_button <- tcltk::tkbutton(tt, text = " OK ", command = on_okay)
  tcltk::tkbind(pwd_input, "<Return>", on_okay)
  tcltk::tkgrid(ok_button, columnspan = 2, pady = 5)
  
  tcltk::tkfocus(tt)
  tcltk::tkwait.window(tt)
  
}