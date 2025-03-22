*******************************
*------- Assignment -------*
*---------- GROUP n ----------*
* nome cognome - student id
* nome cognome - student id
* Jovana Mrdalj - 3280610
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
if ("`user'" == "Jovana") {
    global filepath `"C:\Users\Jovana\OneDrive - Università Commerciale Luigi Bocconi\Desktop\Master\Y1S2\European Integration\Take Home\data"'
}

if ("`user'" == "user") {
    global filepath "/Users/user/Desktop/STATA/eu/" //emanuela's file path
}

// Set directory
cd "$filepath"

* Defines paths and folders 
global data "$filepath"
global output "$filepath\output"
global temp "$filepath\output\temp"

*--------------------------------------*
*------------Question 1.a--------------*
