/*************************************************
*    The PMW Music Typesetter - 3rd incarnation  *
*************************************************/

/* Copyright (c) Philip Hazel, 1991 - 2008 */

/* Written by Philip Hazel, starting November 1991 */
/* This file last modified: September 2008 */


/* This is the header file for the bar measuring routines. */

/* To re-use one field in the pos structure without having to mess about with
unions, we use this macro. */

#define posstaves stemup[0]

/* Flag bits for remembering what precedes a note */

#define xf_caesura    1
#define xf_comma      2
#define xf_tick       4
#define xf_dotbar     8
#define xf_rrepeat   16
#define xf_clef      32
#define xf_lrepeat   64
#define xf_keytime  128
#define xf_grace    256

extern barposstr *pos_bp;
extern BOOL pos_barstartrepeat;

extern workposstr *pos_insertextras(int, int, int, int *, int *,
  int *, workposstr *, int, int);

extern int pos_typewidth(int, int, int);

extern int pos_notewidth(int);

/* End of poshdr.h */
