*******************************
*------- Assignment -------*
*---------- GROUP n ----------*
* nome cognome - student id
* nome cognome - student id
* nome cognome - student id
* Emanuela Narduzzi - 3173310
*******************************
clear all
set more off

*----------------------------------------------------------------*
**************************---QUESTION 1---************************
*----------------------------------------------------------------*
/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "username") {
    global filepath ""/Users/username/.../"  //insert here your username and file path
}

if ("`user'" == "user") {
    global filepath "/Users/user/Desktop/STATA/micro/files/" //emanuela's file path
}

// Set directory
cd "$filepath"

*--------------------------------------*
*------------Question 1.a--------------*
