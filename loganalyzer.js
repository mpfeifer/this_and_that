/*
 * loganalyzer.js
 * Started on Donnerstag, 16 März 2017.
 */

$( document ).ready(function() {
    $( ".logline-light" ).click( function() {
        $(this).toggleClass( "logline-highlighted" );
    });
    $( ".logline-dark" ).click( function() {
        $(this).toggleClass( "logline-highlighted" );
    });

});
