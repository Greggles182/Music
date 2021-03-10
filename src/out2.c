/*************************************************
*    The PMW Music Typesetter - 3rd incarnation  *
*************************************************/

/* Copyright (c) Philip Hazel, 1991 - 2021 */

/* Written by Philip Hazel, starting November 1991 */
/* This file last modified: January 2021 */


/* This file contains miscellaneous subroutines for outputting a page */


#include "pmwhdr.h"
#include "pagehdr.h"
#include "outhdr.h"

/* Clef style can currently take values 0, 1, 2, or 3; the columns below
correspond to this. */

static int clef_chars[] = {
  mc_trebleclef,    mc_trebleclef,       mc_trebleclef,       mc_trebleclef,
  mc_sopranoclef,   mc_sopranoclef,      mc_oldsopranoclef,   mc_oldsopranoclef,
  mc_mezzoclef,     mc_mezzoclef,        mc_oldmezzoclef,     mc_oldmezzoclef,
  mc_altoclef,      mc_altoclef,         mc_oldaltoclef,      mc_oldaltoclef,
  mc_tenorclef,     mc_tenorclef,        mc_oldtenorclef,     mc_oldtenorclef,
  mc_cbaritoneclef, mc_oldcbaritoneclef, mc_cbaritoneclef,    mc_oldcbaritoneclef,
  mc_baritoneclef,  mc_oldbaritoneclef,  mc_baritoneclef,     mc_oldbaritoneclef,
  mc_bassclef,      mc_oldbassclef,      mc_bassclef,         mc_oldbassclef,
  mc_deepbassclef,  mc_olddeepbassclef,  mc_deepbassclef,     mc_olddeepbassclef,
  mc_hclef,         mc_hclef,            mc_hclef,            mc_hclef,
  0,                0,                   0,                   0,
  mc_trebleDclef,   mc_trebleDclef,      mc_trebleDclef,      mc_trebleDclef,
  mc_trebleTclef,   mc_trebleTclef,      mc_trebleTclef,      mc_trebleTclef,
  mc_trebleTBclef,  mc_trebleTBclef,     mc_trebleTBclef,     mc_trebleTBclef,
  mc_Sbassclef,     mc_oldSbassclef,     mc_Sbassclef,        mc_oldSbassclef,
  mc_Cbassclef,     mc_oldCbassclef,     mc_Cbassclef,        mc_oldCbassclef
};

static uschar clef_adjusts[] = { 4, 0, 4, 8, 12, 16, 8, 12, 16, 0, 0, 4, 4, 4, 12, 12 };

/* These are the clef adjustments for custom key signatures. */

static int xclef_adjusts[] = {
  -1,   /* treble */
  -6,   /* soprano */
  -4,   /* mezzo */
  -2,   /* alto */
   0,   /* tenor */
   2,   /* cbaritone */
  -5,   /* baritone */
  -3,   /* bass */
  -1,   /* deepbass */
  -1,   /* h-clef */
  -1,   /* none */
  -1,   /* trebledescant */
  -1,   /* trebletenor */
  -1,   /* trebletenorB */
  -3,   /* soprabass */
  -3    /* contrabass */
};


/************************************************
*         Output a stave joining sign           *
************************************************/

/* This procedure is used for outputting lines, brackets, or braces at the
start of a system. It returns a bit map of the staves it referenced. For braces
and thin brackets, prev is the bit map of the (thick) bracketed staves. */


/* Local subroutine to determine if a stave is being printed.

Arguments:
  stave      stave number
  bar        bar number

Returns:     TRUE if the bar is to be printed
*/

static BOOL
is_printing(int stave, int bar)
{
stavestr *ss = curmovt->stavetable[stave];
return mac_teststave(out_sysblock->notsuspend, stave) &&
  (!ss->omitempty || (ss->barindex)[bar] != NULL);
}


/* If yield is not null, bits are set for those staves that are included in the
joining item.

Arguments:
  list       pointer to chain of stave selections
  prev       bitvector of previous thick bracket for thin bracket and brace;
               can be NULL for other types (not used)
  which      type of join sign, e.g. join_brace
  bartype    barline type when which == join_barline
  bar        number of the first bar in the system
  yield      a stave bitvector, or NULL

Returns:     nothing
*/

void
out_dojoinsign(stave_list *list, usint *prev, int which, int bartype, int bar,
  usint *yield)
{
if (yield != NULL) mac_initstave(yield, 0);

for (; list != NULL; list = list->next)
  {
  int pb1 = list->first;
  int pb2 = list->last;

  if (pb1 > curmovt->laststave) continue;
  if (pb2 > curmovt->laststave) pb2 = curmovt->laststave;
  while (pb1 < out_laststave && pb1 < pb2 && !is_printing(pb1, bar)) pb1++;
  while (pb2 > pb1 && !is_printing(pb2, bar)) pb2--;

  if (is_printing(pb1, bar) || is_printing(pb2, bar))
    {
    int i, xx;
    stavestr *ss = curmovt->stavetable[pb1];

    mac_setstavesize(pb1);
    xx = main_stavemagn * ((ss->stavelines == 6)? 4 : (ss->stavelines == 4)? -4 : 0);

    switch(which)
      {
      case join_thinbracket:
      case join_brace:

      /* Should never be printed for one stave; there is some old code
      that gets a brace positioned right if it ever is, but at present that
      code is never triggered. */

      if (out_depthvector[pb1] != out_depthvector[pb2])
        {
        BOOL overlap = FALSE;
        for (i = pb1; i <= pb2; i++)
          if (mac_teststave(prev, i)) { overlap = TRUE; break; }

        if (which == join_brace)
          {
          int adjust = (out_depthvector[pb1] == out_depthvector[pb2])? 7000:8500;
          if (overlap) adjust += 1500;
          ps_brace(out_joinxposition - adjust,
            out_yposition + out_depthvector[pb1] - xx,
              out_yposition + out_depthvector[pb2], main_stavemagn);
          }
        else    /* thin bracket */
          {
          int x[4], y[4];
          out_ystave = out_yposition;
          x[0] = x[3] = out_joinxposition;
          x[1] = x[2] = x[0]  - (overlap? 2500:1000) - 3000;
          y[0] = y[1] = - out_depthvector[pb1] + 16*main_stavemagn + xx;
          y[2] = y[3] = - out_depthvector[pb2];
          ps_lines(x, y, 4, 400);
          }
        }
      break;

      case join_bracket:
      ps_bracket(out_joinxposition-3500, out_yposition + out_depthvector[pb1] - xx,
        out_yposition + out_depthvector[pb2], main_stavemagn);
      break;

      case join_barline:
      ps_barline(out_joinxposition, out_yposition + out_depthvector[pb1] - xx,
        out_yposition + out_depthvector[pb2], bartype);
      break;
      }

    if (yield != NULL) for (i = pb1; i <= pb2; i++) mac_setstave(yield, i);
    }
  }
}




/************************************************
*               Output a clef                   *
************************************************/

/* The clef characters all print a little to the right of the given position.
The reason is historical. It does not matter at all, except when staves of
different sizes are printed - then the different size causes an uneven spacing
at the start of a line. So attempt to correct for this.

We must also adjust the vertical position for clefs that are not of full size,
so that they appear at the correct position on the stave.

Arguments:
  x           x-position
  y           y-position
  clef        which clef
  size        size
  midbar      TRUE if a mid-bar clef

Returns:      nothing
*/

void
out_writeclef(int x, int y, int clef, int size, BOOL midbar)
{
switch(clef)
  {
  case clef_none:
  return;

  case clef_cbaritone:
  case clef_tenor:
  case clef_alto:
  case clef_soprano:
  case clef_mezzo:
  x += (2000 * (1000 - main_stavemagn))/1000;
  break;

  case clef_baritone:
  case clef_deepbass:
  case clef_bass:
  case clef_contrabass:
  case clef_soprabass:
  x -= ((midbar? 15:5)*main_stavemagn)/10;    /* move left 0.5 pt or 1.5 pt in mid bar */
  x += (750 * (1000 - main_stavemagn))/1000;
  break;

  case clef_h:
  break;

  case clef_treble:
  case clef_trebletenor:
  case clef_trebletenorB:
  case clef_trebledescant:
  x += (700 * (1000 - main_stavemagn))/1000;
  break;
  }

ps_muschar(x,
  y - mac_muldiv(10000 - size, clef_adjusts[clef]*main_stavemagn, 10000),
    clef_chars[clef*4 + curmovt->clefstyle], (size*main_stavemagn)/1000);
}



/************************************************
*              Output a key                     *
************************************************/

/* This handles both standard and custom keys, taking note of any "printkey"
settings. The 0x40 bit is set when a naturalizing key is required.

Arguments:
  x         x-position
  y         y-position
  clef      the current clef
  key       the key signature

Returns:    nothing
*/

void
out_writekey(int x, int y, int clef, int key)
{
int i, ch, chwidth, n, *order;
int key63 = key & 63;
BOOL use_naturals = (key & 0x40) != 0;
pkeystr *pk;

for (pk = main_printkey; pk != NULL; pk = pk->next)
  {
  if (key63 == pk->key && clef == pk->clef &&
      pk->movt_number <= curmovt->number)
    break;
  }

/* There is a special string for this key. */

if (pk != NULL)
  {
  out_string((key > 63)? pk->cstring : pk->string, font_mf, 10*main_stavemagn,
    x, y, 0);
  return;
  }

/* Handle standard key signature printing */

if (key63 < X_key)
  {
  if ((n = main_keysigtable[key63]) > 0)
    {
    ch = mc_sharp;
    chwidth = (curmovt->accspacing)[ac_sharp];
    order = main_sharporder;
    }
  else
    {
    ch = mc_flat;
    chwidth = (curmovt->accspacing)[ac_flat];
    order = main_flatorder;
    n = -n;
    }

  if (use_naturals)
    {
    ch = mc_natural;
    chwidth = (curmovt->accspacing)[ac_natural];
    }

  order += main_clefoffset[clef];
  chwidth = (chwidth * main_stavemagn)/1000;

  for (i = 0; i < n; i++)
    {
    ps_muschar(x, y-(order[i]*main_stavemagn)/1000, ch, 10*main_stavemagn);
    x += chwidth;
    }
  }

/* Handle custom key signature. To preserve the accidental order in which the
key was defined, we use an additional vector, which can be terminated early by
a value greater than 9. */

else
  {
  uschar *p = main_xkeys[key63 - X_key];
  int clef_offset = xclef_adjusts[clef];

  for (i = 0; i < 10; i++)
    {
    int level, offset, baseacc;
    int k = main_xkeyorder[key63 - X_key][i];

    if (k > 9) break;       /* Early end of order list (normally the case) */
    baseacc = p[k] & 0x7f;

    level = k + clef_offset;
    if (level > 9) level -= 7; else if (level < 0) level += 7;

    if (use_naturals)
      {
      offset = baseacc = ac_natural;
      }
    else
      {
      offset = baseacc;
      if ((p[k] & 0x80) != 0)
        offset += ((offset == ac_sharp)? (curmovt->hsharpstyle == 0) :
          (curmovt->hflatstyle == 0))? 18:36;
      }

    ps_muschar(x, y-(level*2*main_stavemagn), out_acctable[offset],
      10*main_stavemagn);
    x += (curmovt->accspacing[baseacc] * main_stavemagn)/1000;
    }
  }
}



/************************************************
*              Output a time                    *
************************************************/

/*
Arguments:
  x         x-position
  y         y-position
  ts        time signature

Returns:    nothing
*/

void
out_writetime(int x, int y, int ts)
{
ptimestr *pt = main_printtime;
int *fontvector = (curmovt->fontsizes)->fontsize_text;
int offsetn, offsetd, sizen, sized;
uschar vn[16];
uschar vd[16];
uschar *topstring = vn;
uschar *botstring = vd;

/* If not printing time signatures, return */

if (!curmovt->showtime) return;

/* First see if this time signature has special strings specified for its
printing. The printtime directive must have happened in this movement or
earlier for it to be applicable. */

while (pt != NULL)
  {
  if (pt->time == ts && pt->movt_number <= curmovt->number) break;
  pt = pt->next;
  }

/* If found special case, get strings and sizes from it */

if (pt != NULL)
  {
  offsetn = pt->offsettop;
  offsetd = pt->offsetbot;
  topstring = pt->top;
  botstring = pt->bot;
  }

/* Default printing for this time signature. First mask off the multiplier,
then check for the special cases of C and A. */

else
  {
  ts &= 0xFFFF;

  /* C and A are special cases */

  if (ts == time_common || ts == time_cut)
    {
    ps_muschar(x, y - 4*main_stavemagn,
      ((ts == time_common)? mc_common : mc_cut), 10*main_stavemagn);
    return;
    }

  /* Non-special case - set up numerator and denominator, in the
  time signature font. */

  sprintf(CS vn, "%d", ts >> 8);
  sprintf(CS vd, "%d", ts & 255);
  offsetn = offsetd = ff_offset_ts;
  }

/* We now have in topstring and botstring two strings to print. Arrange that
they are centred with respect to each other when both are to be printed. Also
arrange to adjust the heights according to the font size. We assume that at
12-points, the height is 8 points, which is true for the default bold font.
However, it is not true for the music font, so there is a fudge to check for
that case which will catch the common cases. This is all rather
unsatisfactory... */

sizen = (fontvector[offsetn]*main_stavemagn)/1000;
sized = (fontvector[offsetd]*main_stavemagn)/1000;

if (curmovt->showtimebase && botstring[0] != 0)
  {
  int stdsize = 12;
  int nx = 0;
  int dx = 0;
  int widthn = string_width(topstring, font_bf, sizen);
  int widthd = string_width(botstring, font_bf, sized);

  if (widthn > widthd) dx = (widthn - widthd)/2; else
    nx = (widthd - widthn)/2;

  out_string(topstring, curmovt->font_time, sizen, x+nx,
    y - (8000*main_stavemagn)/1000, 0);

  if (Ustrncmp(botstring, "\\mf\\", 4) == 0) stdsize = 10;
  out_string(botstring, curmovt->font_time, sized, x+dx,
    y + ((3*(sized-stdsize*main_stavemagn)/4)*main_stavemagn)/1000, 0);
  }

else
  {
  int stdsize = 12;
  if (Ustrncmp(topstring, "\\mf\\", 4) == 0) stdsize = 10;
  out_string(topstring, curmovt->font_time, sizen, x,
    y - ((4000 - (sizen-stdsize*main_stavemagn)/3)*main_stavemagn)/1000, 0);
  }
}




/*************************************************
*           Output repeat marks                  *
*************************************************/

static int repspacing[] = {

/* righthand repeats */
/* thick  thin  dots */
    50,    31,    6,        /* repeatstyle = 0 */
    -1,    50,   25,        /* repeatstyle = 1 */
    -1,    50,   25,        /* repeatstyle = 2 */
    -1,    -1,   25,        /* repeatstyle = 3 */
    50,    31,    6,        /* repeatstyle = 4 */

/* lefthand repeats */
/* thick  thin  dots */
     0,    35,   51,        /* repeatstyle = 0 */
    -1,     0,   15,        /* repeatstyle = 1 */
    -1,     0,   15,        /* repeatstyle = 2 */
    -1,    -1,   15,        /* repeatstyle = 3 */
     0,    35,   51,        /* repeatstyle = 4 */

/* righthand double repeats */
/* thick  thin  dots */
    50,    31,    6,        /* repeatstyle = 0 */
    -1,    50,   25,        /* repeatstyle = 1 */
    -1,    50,   25,        /* repeatstyle = 2 */
    -1,    -1,   25,        /* repeatstyle = 3 */
    34,    -1,    6,        /* repeatstyle = 4 */

/* lefthand double repeats */
/* thick  thin  dots */
     0,    35,   51,        /* repeatstyle = 0 */
    -1,     0,   15,        /* repeatstyle = 1 */
    -1,     0,   15,        /* repeatstyle = 2 */
    -1,    -1,   15,        /* repeatstyle = 3 */
    17,    -1,   51         /* repeatstyle = 4 */
};

/*
Arguments:
  x          x-position
  type       type of repeat
  magn       magnification

Returns:     nothing
*/

void
out_writerepeat(int x, int type, int magn)
{
int *xx = repspacing + type + curmovt->repeatstyle * 3;

if (xx[0] >= 0)
  ps_barline(x + (xx[0]*magn)/10, out_ystave, out_ybarend, bar_thick);

if (xx[1] >= 0)
  ps_barline(x + (xx[1]*magn)/10, out_ystave, out_ybarend,
    (curmovt->repeatstyle == 2)? bar_dotted : bar_single);

out_string((curmovt->repeatstyle != 3)? US"xI" : US"IxxyyyyyyI", font_mf,
  10*main_stavemagn, x + (xx[2]*magn)/10 + (65*(magn - main_stavemagn))/100,
  out_ystave, 0);
}




/*************************************************
*        Find X offset for given M offset        *
*************************************************/

/* The search starts at the current out_posptr. We never search for a value
that is less than a previous one. When setting up a beam over a bar line, the
moff can be greater than the bar length. In this case, and also in the case
when it is equal to the bar length, we must search the NEXT bar.

Argument:  the musical offset in the bar
Returns:   the x offset in the bar
*/

int
out_findXoffset(int moff)
{
if (!beam_overbeam || moff < out_poslast->moff)
  {
  while (moff > out_posptr->moff && out_posptr < out_poslast) out_posptr++;
  while (moff < out_posptr->moff && out_posptr > out_postable) out_posptr--;
  if (moff == out_posptr->moff) return out_posptr->xoff;
  }

/* Handle the beam over bar line case */

else
  {
  int newmoff = moff - out_poslast->moff;
  posstr *new_postable, *new_posptr, *new_poslast;
  barposstr *bp = curmovt->posvector + out_bar + 1;
  new_postable = new_posptr = bp->vector;
  new_poslast = new_postable + bp->count - 1;

  while (newmoff > new_posptr->moff && new_posptr < new_poslast) new_posptr++;
  while (newmoff < new_posptr->moff && new_posptr > new_postable) new_posptr--;

  if (newmoff == new_posptr->moff)
    return new_posptr->xoff + out_poslast->xoff + out_sysblock->barlinewidth;
  }

/* Cannot find a position for this moff. This error is hard, but need to keep
the compiler happy. */

error_moan(ERR60, out_bar, out_stave, moff, moff);
return 0;
}



/*************************************************
*  Find X offset for one of two given M offsets  *
*************************************************/

/* This is used only in the non-overbeaming case. It returns the x offset of
the first moff if it exists, otherwise the x offset of the second moff.

Arguments:
  moff1       the first music offset
  moff2       the second music offset

Returns:      the x offset
*/

int
out_findGoffset(int moff1, int moff2)
{
while (moff1 > out_posptr->moff && out_posptr < out_poslast) out_posptr++;
while (moff1 < out_posptr->moff && out_posptr > out_postable) out_posptr--;

if (moff1 == out_posptr->moff) return out_posptr->xoff;
return out_findXoffset(moff2);
}



/*************************************************
*     Find postable entry for given M offset     *
*************************************************/

/*
Argument: the music offset
Returns:  pointer to the postable entry, or NULL if not found
*/

posstr *
out_findTentry(int moff)
{
while (moff > out_posptr->moff && out_posptr < out_poslast) out_posptr++;
while (moff < out_posptr->moff && out_posptr > out_postable) out_posptr--;
return (moff == out_posptr->moff)? out_posptr : NULL;
}



/*************************************************
*   Find X offset or interpolate in current bar  *
*************************************************/

/* This is used to implement MusicXML-styole "offsets", which may be positive
or negative. Don't alter out_posptr. If we don't find an exact moff,
interpolate between two entries or, if off the end, fudge assuming 16 points
per crotchet.

Argument: the music offset
Returns:  an x offset
*/

int
out_findAoffset(int moff)
{
posstr *a, *b;
posstr *p = out_posptr;
while (moff > p->moff && p < out_poslast) p++;
while (moff < p->moff && p > out_postable) p--;
if (moff == p->moff) return p->xoff;

if (moff > p->moff)
  {
  if (p == out_poslast)  /* After last entry */
    return p->xoff + mac_muldiv(16000, moff - p->moff, len_crotchet);
  a = p;
  b = p + 1;
  }

else
  {
  if (p == out_postable)  /* Before first entry */
    return p->xoff - mac_muldiv(16000, p->moff - moff, len_crotchet);
  b = p;
  a = p - 1;
  }

/* In between two entries; interpolate. */

return a->xoff +
  mac_muldiv(b->xoff - a->xoff, moff - a->moff, b->moff - a->moff);
}

/* End of out2.c */
