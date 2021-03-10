/*************************************************
*    The PMW Music Typesetter - 3rd incarnation  *
*************************************************/

/* Copyright (c) Philip Hazel, 1991 - 2020 */

/* Written by Philip Hazel, starting November 1991 */
/* This file last modified: April 2020 */


/* This file contains code for setting up and outputting beams */


#include "pmwhdr.h"
#include "pagehdr.h"
#include "readhdr.h"
#include "outhdr.h"



/*************************************************
*              Static variables                  *
*************************************************/


static int retryslopes[] = { 100, 50, 0, -50, -100, -300 };

static int minoffset;




/*************************************************
*           Determine slope for beam             *
*************************************************/

/* This procedure is called only when there are more than two notes in the
beam. It is a totally heuristic function, but it seems to produce reasonable
slopes pretty nearly all the time.

Arguments:   none
Returns:     slope
*/

static int
findslope(void)
{
b_notestr *left = beam_first;
int leftpitch = left->spitch;
int rightpitch = beam_last->spitch;
int slope, thisrun, runindex, lastrunindex, lasttype;
int interval;
int lastinterval = 0;
int thispitch, nextpitch, max, min, firstslope;
int runs[3];
BOOL downs_less_than_up, ups_less_than_down;

mac_couplepitch(leftpitch, left->flags);
mac_couplepitch(rightpitch, beam_last->flags);

/* If the two ends of the beam are at the same pitch, draw a level beam. */

if (leftpitch == rightpitch) return 0;

/* Otherwise do some analysis */

firstslope = beam_upflag? (-1) : (+1);   /* in case rest first */
slope = thisrun = 0;
thispitch = leftpitch;
lastrunindex = -1;
lasttype = left->notetype;
max = 0;
min = BIGNUMBER;
runs[0] = runs[1] = runs[2] = 0;                 /* equal, up, down */
downs_less_than_up = ups_less_than_down = TRUE;

while (left != beam_last)
  {
  b_notestr *next = misc_nextnote(left, NULL);
  nextpitch = next->spitch;

  /* Totally ignore grace/non-grace as required */

  if ((out_gracenotes && next->length != 0) ||
      (!out_gracenotes && next->length == 0))
    {
    left = next;
    continue;
    }

  /* Skip over rests, but keep the note type */

  if (nextpitch != 0)
    {
    mac_couplepitch(nextpitch, next->flags);
    if (left == beam_first) firstslope = nextpitch - thispitch;
    if (next != beam_last)
      {
      if (nextpitch > max) max = nextpitch;  /* Max/min of all but the first */
      if (nextpitch < min) min = nextpitch;  /* and last notes */
      }

    interval = nextpitch - thispitch;
    runindex = (interval == 0)? 0 : (interval > 0)? 1 : 2;

    /* Slope is same as previous */

    if (runindex == lastrunindex)
      {
      if (next->notetype >= lasttype || (beam_upflag && runindex == 1) ||
        (!beam_upflag && runindex == 2))
          thisrun++;
      }

    /* Slope is different from previous */

    else
      {
      if (lastrunindex >= 0)
        {
        if (thisrun > runs[lastrunindex]) runs[lastrunindex] = thisrun;
        if (interval > 0 && interval >= abs(lastinterval))
          ups_less_than_down = FALSE;
        if (interval < 0 && abs(interval) >= lastinterval)
          downs_less_than_up = FALSE;
        }
      thisrun = 1;
      lastrunindex = runindex;
      }

    /* Save for next note */

    lastinterval = interval;
    thispitch = nextpitch;
    }

  lasttype = next->notetype;
  left = next;
  }

if (lastrunindex >= 0 && thisrun > runs[lastrunindex])
  runs[lastrunindex] = thisrun;

/* This is the heuristic algorithm that produces a default slope */

if (runs[1] > runs[0] && runs[1] > runs[2]) slope = curmovt->maxbeamslope2;
  else if (runs[2] > runs[0] && runs[2] > runs[1]) slope = - curmovt->maxbeamslope2;

/* Some special cases override the heuristic */

if (leftpitch > rightpitch)
  {
  if (beam_upflag && leftpitch - rightpitch >= 4 && max <= rightpitch + 2)
    slope = - curmovt->maxbeamslope2;
  else if (!beam_upflag && min <= rightpitch) slope = 0;
  else if (slope > 0) slope = 0;
  }
else
  {
  if (!beam_upflag && rightpitch - leftpitch >= 4 && min >= rightpitch - 2)
    slope = curmovt->maxbeamslope2;
  else if (beam_upflag && max >= rightpitch) slope = 0;
  else if (slope < 0) slope = 0;
  }

if (beam_upflag)
  {
  if (slope < 0 && (firstslope >= 0 || runs[1] > 2 || !ups_less_than_down))
    return 0;
  }
else
  {
  if (slope > 0 && (firstslope <= 0 || runs[2] > 2 || !downs_less_than_up))
    return 0;
  }

return slope;
}



/*************************************************
*             Compute Beam Offset                *
*************************************************/

/* The original code dealt only with beams where all the notes had stems in the
same direction. The addition of logic to deal with other cases leaves this code
alone, but adds additional computations to compute the two offsets for the two
sets of notes. If this is successful, then a middle position is chosen at the
end.

Arguments:
  ornament       the first note's ornament, or -1
  lastopposite   TRUE if the last note's stem goes the opposite way
  adjusts        x-adjustments for the notes
  longestnote    longest note in the beam

Returns:         FALSE if notes on opposite sides are wanted, but cannot be
                   handled; otherwise TRUE.
*/

static BOOL
ComputeBeamOffset(int ornament, BOOL lastopposite, int *adjusts,
  int longestnote)
{
b_notestr *thisnote = beam_first;
BOOL forcelength = TRUE;
int diff_thisoffset;
int diff_thatoffset = BIGNUMBER;            /* => unset */
int hemidemisemiquavers = FALSE;
int moff = beam_firstmoff;
int adjustptr = 0;
int xfirst = out_findXoffset(beam_firstmoff) + adjusts[0];
int yadjust = beam_first->yextra;
int yfirstpitch = beam_first->spitch;
int yfirst;

mac_couplepitch(yfirstpitch, beam_first->flags);
yfirst = (yfirstpitch - 130)*main_stavemagn;
beam_splitOK = TRUE;

/* Initialize from the first note, forcing a stem length if not coupled. */

if (beam_upflag)
  {
  beam_offset = diff_thisoffset = yadjust;
  if ((beam_first->flags & nf_coupleD) == 0)
    {
    forcelength = FALSE;
    if (yfirstpitch < 122)
      beam_offset += (122 - yfirstpitch)*main_stavemagn;
    }
  }
else
  {
  beam_offset = diff_thisoffset = -yadjust;
  if ((beam_first->flags & nf_coupleU) == 0)
    {
    forcelength = FALSE;
    if (yfirstpitch > 150)
      beam_offset -= (yfirstpitch - 150)*main_stavemagn;
    }
  }

minoffset = abs(beam_offset) + 8000;

/* Loop through the beam */

for (;;)
  {
  BOOL note_upflag = (thisnote->flags & nf_stemup) != 0;
  int xnote = out_findXoffset(moff) -
    ((lastopposite && thisnote == beam_last)? beam_Xcorrection : 0) +
      adjusts[adjustptr];
  int thatynote;
  int ybasic = yfirst + mac_muldiv(beam_slope, xnote-xfirst, 1000);
  int ybeam = ybasic + beam_offset;
  int thisybeam = ybasic + diff_thisoffset;
  int ypitch = thisnote->spitch;
  int ynote;

  mac_couplepitch(ypitch, thisnote->flags);

  yadjust = thisnote->yextra;

  if (ornament == or_trem2)
    yadjust += ((ypitch & 2) == 0)? 4000 : 2000;

  else if (ornament == or_trem3)
    yadjust += ((ypitch & 2) == 0)? 8000 : 6000;

  ynote = (ypitch - 130)*main_stavemagn + n_upfactor*yadjust;

  /* If this is the first note of the beam, it might really be a rest in
  disguise. We can tell this from the true pitch. If so, adjust for
  longestnote. */

  if (thisnote->truepitch == 0)
    {
    ynote = ynote + n_upfactor*(longestnote - quaver)*4*main_stavemagn;
    }

  /* Else if this is the first non-coupled note, force a stem length. We don't
  force for more than one note in a beam, because that leads to excessively
  long stems. */

  else if (forcelength)
    {
    if (beam_upflag)
      {
      if ((thisnote->flags & nf_coupleD) == 0)
        {
        forcelength = FALSE;
        if (ynote < L_0L - 2000) ynote += (L_0L - 2000) - ynote;
        }
      }
    else
      {
      if ((thisnote->flags & nf_coupleU) == 0)
        {
        forcelength = FALSE;
        if (ynote > L_6L + 2000) ynote -= ynote - (L_6L+2000);
        }
      }
    }

  thatynote = ynote - 2*n_upfactor*yadjust;

  if (thisnote->notetype == hdsquaver) hemidemisemiquavers = TRUE;

  if (beam_upflag)
    {
    int m;
    if (ynote > ybeam) beam_offset += ynote - ybeam;
    m = ybasic + beam_offset - ynote + yadjust + 8000;
    if (m < minoffset) minoffset = m;
    }
  else
    {
    int m;
    if (ynote < ybeam) beam_offset -= ybeam - ynote;
    m = ynote - (ybasic + beam_offset) + yadjust + 8000;
    if (m < minoffset) minoffset = m;
    }

  if (note_upflag == beam_upflag)
    {
    if (beam_upflag)
      {
      if (ynote > thisybeam) diff_thisoffset += ynote - thisybeam;
      }
    else if (ynote < thisybeam) diff_thisoffset -= thisybeam - ynote;
    }

  else if (beam_splitOK)
    {
    int thatybeam;

    /* If this is the first opposed note, calculate an initial offset, else
    adjust the offset. */

    if (diff_thatoffset == BIGNUMBER)
      diff_thatoffset = (thatynote - n_upfactor*28000) - ybasic;
    else
      {
      thatybeam = ybasic + diff_thatoffset + n_upfactor*28000;
      if (!beam_upflag)
        {
        if (thatynote > thatybeam) diff_thatoffset += thatynote - thatybeam;
        }
      else
        if (thatynote < thatybeam) diff_thatoffset -= thatybeam - thatynote;
      }
    }

  /* If this is a split beam, check that it's still possible */

  if (beam_splitOK && diff_thatoffset != BIGNUMBER &&
    ((beam_upflag && diff_thatoffset < diff_thisoffset) ||
      (!beam_upflag && diff_thatoffset > diff_thisoffset)))
        return FALSE;

  /* We're done if this is the last note */

  if (thisnote == beam_last) break;

  /* Advance to next note, skipping grace notes unless we are doing a grace
  note beam. If we hit a rest, deal with it and then if it is the last note of
  the beam, exit from both loops */

  for (;;)
    {
    adjustptr++;
    moff += thisnote->length;
    thisnote = misc_nextnote(thisnote, &ornament);

    if (out_gracenotes) moff++;
      else if (thisnote->length == 0) continue;

    /* Not a rest - go round the outer loop again */

    if (thisnote->spitch != 0) break;

    /* Deal with rests */

    xnote = out_findXoffset(moff) - adjusts[adjustptr];
    ybasic = yfirst + mac_muldiv(beam_slope, xnote-xfirst, 1000);
    ybeam = ybasic + beam_offset;

    if (beam_upflag)
      {
      int yrest = (thisnote->yextra*main_stavemagn)/1000;
      if (thisnote->notetype > squaver) yrest += 4*main_stavemagn;
      if (longestnote > quaver)
        yrest += (longestnote - quaver)*4*main_stavemagn;
      if (yrest > ybeam) beam_offset += yrest - ybeam;
      }
    else
      {
      int yrest = 12*main_stavemagn + (thisnote->yextra*main_stavemagn)/1000;
      if (thisnote->notetype > quaver) yrest -= 4*main_stavemagn;
      if (longestnote > quaver)
        yrest -= (longestnote - quaver)*4*main_stavemagn;
      if (yrest < ybeam) beam_offset -= ybeam - yrest;
      }

    /* At the end of the beam, break out of 2 loops. Otherwise, go round the
    inner loop again. */

    if (thisnote == beam_last) goto ENDBEAMLOOP;
    }
  }

ENDBEAMLOOP:

/* Hemidemisemiquavers require some additional space */

if (hemidemisemiquavers)
  {
  beam_offset += n_upfactor*1000;
  diff_thisoffset += n_upfactor*1000;
  if (diff_thatoffset != BIGNUMBER) diff_thatoffset -= n_upfactor*1000;
  }

/* If notes on the other side of the beam have been successfully processed, use
the compromise offset. The flag is still TRUE if none have been encountered, so
we set it FALSE to save wasting time with additional processing in later
routines. */

if (beam_splitOK && diff_thatoffset != BIGNUMBER)
  beam_offset = (diff_thisoffset + diff_thatoffset)/2;
    else beam_splitOK = FALSE;    /* this is the normal case! */

return TRUE;
}



/*************************************************
*              Draw a set of beams               *
*************************************************/

/* The coordinates for the start of the beam are in global variables (for
use when computing stem lengths).

Arguments:
  xright         x-coordinate for the right-hand end
  adjusts        the x-adjusts for the notes
  longestnote    the longest note in the beam
  lastopposite   TRUE if the last note is on the opposite side
  rightbreak2    flag for funny stub (see comment below)

Returns:         nothing
*/

static void
DrawBeams(int xright, int *adjusts, int longestnote, BOOL lastopposite,
  BOOL rightbreak2)
{
int level, i;
int lastlen = (len_minim*out_pnum)/out_pden;
int throughlevel = longestnote - quaver + 1;
int thisnotetype = longestnote + 1;
int sladjust = mac_muldiv(75*main_stavemagn, n_fontsize, 1000000) + n_cueadjust;
int sradjust = mac_muldiv(115*main_stavemagn, n_fontsize, 1000000) + n_cueadjust;

uschar taildirflags[MAX_BEAMSIZE];

/* Add possible cue note adjustment into endpoints */

beam_firstX += n_cueadjust;
xright += n_cueadjust;

/* Adjust parameters for cue or grace notes */

if (n_fontsize != 10000)
  {
  int adjust = (14*(10000 - n_fontsize))/10;
  if (beam_upflag) beam_firstY -= adjust;
    else beam_firstY += adjust;
  }

/* If we have drawn more than one beam, and there are notes on the other side
of it, we must lengthen all of their stems. This code is obeyed even for one
beam, as it then correctly sets the values to zero. */

if (beam_splitOK)
  {
  int adjust = (throughlevel-1)*3;
  b_notestr *p = beam_first;

  for (i = beam_count; i >= 1 ; i--)
    {
    beam_stemadjusts[i] =
      (((p->flags & nf_stemup) != 0) == beam_upflag)? 0 : adjust;
    p = misc_nextnote(p, NULL);
    while (p != NULL && p->spitch == 0)
      {
      p = misc_nextnote(p, NULL);
      i--;
      }
    }
  }

/* If this is an accellerando or ritardando beam, just draw the appropriate
lines; there should not be any beamlets, etc. The number of lines is in the
beam_accrit variable (2 or 3); its sign indicates an acc or a rit. */

if (beam_accrit != 0)
  {
  int count = abs(beam_accrit);
  int sign = beam_accrit/count;

  for (i = 0; i < count; i++)
    {
    ps_beam(beam_firstX, xright, 1, i*sign);
    }
  beam_accrit = 0;
  return;
  }

/* "Normal" beams - first draw beams which go right through all notes */

for (i = 1; i <= throughlevel; i++)
  {
  ps_beam(beam_firstX, xright, i, 0);
  lastlen /= 2;
  }

/* Clear the flags which remember about subtail directions and opposites */

for (i = 0; i <= beam_count; i++) taildirflags[i] = 0;

/* Now scan the notes and add internal beams or beamlets - thisnotetype
contains the notetype for the beam under consideration. */

for (level = throughlevel+1; level <= 4; level++)
  {
  b_notestr *left = beam_first;
  b_notestr *postleft = left;

  int donesomething = FALSE;
  int notenumber = 1;
  int leftmoff = beam_firstmoff;
  int rightmoff = leftmoff;
  int adjustptr = 0;
  int firstbreak2 = FALSE;
  int leftdone = FALSE;
  int leftextended = FALSE;
  int rightextended = FALSE;

  /* Determine whether there is a second-level break after the very first note
  - this is a special (unusual) case */

  do mac_advancechord(postleft); while (postleft->type == b_chord);
  if (postleft->type == b_tie)
    {
    postleft = (b_notestr *)((uschar *)postleft + length_table[b_tie]);
    if (postleft->type == b_Jump)
      postleft = (b_notestr *)
        ((uschar *)(((b_Jumpstr *)postleft)->next) + length_table[b_Jump]);
    }
  firstbreak2 = (postleft->type == b_beambreak2);

  /* Now scan the beam */

  while (left != beam_last)
    {
    b_notestr *right = misc_nextnote(left, NULL);
    b_notestr *postright;

    int rightadjustptr = adjustptr + 1;
    int left_up = (left->flags & nf_stemup) != 0;
    int leftnotetype = left->notetype;
    int rightnotetype, right_up;

    rightmoff += left->length;

    /* Rests under beams are totally ignored, except that they must cause a
    beam break if longer than the note on their left; if beamendrests is on, a
    beam can end on a rest. We must also skip over grace notes when not doing a
    grace beam. */

    while ((right->spitch == 0 && right != beam_last) ||
           (right->length == 0 && !out_gracenotes))
      {
      rightmoff += right->length;
      right = misc_nextnote(right, NULL);
      rightadjustptr++;
      }

    /* Set parameters for the right note, and find out if there is a secondary
    break after it. */

    right_up = (right->spitch == 0)? left_up : (right->flags & nf_stemup) != 0;
    rightnotetype = right->notetype;

    postright = right;
    do mac_advancechord(postright); while (postright->type == b_chord);
    if (postright->type == b_tie)
      {
      postright = (b_notestr *)((uschar *)postright + length_table[b_tie]);
      if (postright->type == b_Jump)
        postright = (b_notestr *)
          ((uschar *)(((b_Jumpstr *)postright)->next) + length_table[b_Jump]);
      }
    rightbreak2 = (postright->type == b_beambreak2);

    /* Check for rest beyond right note and see if it requires a beam break. */

    if (right != beam_last)
      {
      b_notestr *afterright = misc_nextnote(right, NULL);
      if (afterright->spitch == 0 &&
        afterright->notetype < rightnotetype) rightbreak2 = TRUE;
      }

    /* If both notes longer than current, do nothing */

    if (leftnotetype >= thisnotetype || rightnotetype >= thisnotetype)
      {
      int skipnote = FALSE;
      int XLadjust = 0;
      int XRadjust = 0;
      int Dlevel = level;

      donesomething = TRUE;    /* note to keep going */

      /* If both notes are shorter or equal to the current notetype, draw a
      complete beam at this level, unless we are at the first note and there is
      a beam break after it, or the notes are on opposite sides of the beam and
      are of different lengths or if the right note is a rest at the end of a
      bar. The right note can be a rest only at the end of the beam, either
      because of a continued beam at system end, or because of beamendrests. In
      the latter case we want to draw complete beamlets. */

      if (leftnotetype >= thisnotetype &&
        rightnotetype >= thisnotetype && !firstbreak2 &&
          (left_up == right_up || leftnotetype == rightnotetype) &&
          (right->spitch != 0 || !beam_overbeam || !out_lineendflag))
        {
        int ns = beam_count - notenumber;   /* for addressing stemadjusts */
        int useopposite = FALSE;
        int xr;

        /* If the first note already has a beamlet drawn on the opposite side,
        then we must use the opposite side. */

        if ((taildirflags[notenumber] & 4) != 0) useopposite = TRUE; else
          {
          /* If both notes are on the opposite side of the beam, draw the the
          beamlet on the opposite side. */

          if (left_up != beam_upflag && right_up != beam_upflag)
            useopposite = TRUE;

          /* If the notes are on different sides of the beam, use the notehead
          side of the left-hand note, except at the very start of a beam. */

          else if (left_up != right_up)
            useopposite = left_up != beam_upflag || left == beam_first;
          }

        /* If using the opposite side, adjust beamlet vertical position and
        flag the notes as having opposite side beamlets. Adjust stem lengths
        where necessary. Note that the stem length adjust vector goes back-
        wards. The value of ns addresses the righthand note. */

        if (useopposite)
          {
          Dlevel = throughlevel - level + 1;
          taildirflags[notenumber]   |= 4;
          taildirflags[notenumber+1] |= 4;

          if (left_up == beam_upflag && !leftextended)
            beam_stemadjusts[ns+1] += 3;

          if (right_up == beam_upflag)
            {
            beam_stemadjusts[ns] += 3;
            rightextended = TRUE;
            }
          }

        /* If not using the opposite side, there may still be stem length
        adjustments for notes with stems the other way. */

        else
          {
          if (left_up != beam_upflag && !leftextended)
            beam_stemadjusts[ns+1] += 3;
          if (right_up != beam_upflag)
            {
            beam_stemadjusts[ns] += 3;
            rightextended = TRUE;
            }
          }

        /* Make adjustments to the x positions in all cases. Note that we test
        for a left-hand rest by the true pitch, as its spitch has been fudged
        to be like a real note. */

        if (left_up && left->truepitch != 0) XLadjust = beam_Xcorrection;
        if (right_up || right->spitch == 0) XRadjust = beam_Xcorrection;

        /* If the right note is a rest, we must be at the end of a beam,
        beaming over a rest, so make the right hand end the same as the end of
        the main beams. */

        xr = (right->spitch == 0)? xright :
          out_findXoffset(rightmoff) + sradjust + adjusts[rightadjustptr] +
            XRadjust;

        /* Now generate the beam */

        ps_beam(
          out_findXoffset(leftmoff) + sladjust + adjusts[adjustptr] + XLadjust,
            xr, Dlevel, 0);
        taildirflags[notenumber]   |= 1;
        taildirflags[notenumber+1] |= 2;
        leftdone = TRUE;
        }

      /* Only one note is shorter than the current note type, or,
      exceptionally, both are, but are on different sides of the beam and are
      of different lengths. Don't draw anything if the relevant note is a rest.
      */

      else
        {
        /* If the left-hand note requires a beam at this level, then give it
        the part beam, pointing right, unless its beam has already been drawn.
        Note that the rest test must be on truepitch. We must set the correct
        flag for the final type, because on the screen the ends get moved
        left/right as a result of it (and printer driver output). */

        if (leftnotetype >= thisnotetype && !leftdone && left->truepitch != 0)
          {
          int x = out_findXoffset(leftmoff) + adjusts[adjustptr] +
            sladjust + (left_up? beam_Xcorrection : 0);

          if (left_up != beam_upflag) Dlevel = throughlevel - level + 1;
          ps_beam(x, x + mac_muldiv(curmovt->beamflaglength, main_stavemagn,
            1000), Dlevel, 0);
          taildirflags[notenumber] |= 1;
          }

        /* If the right hand note requires a beam at this level, give it a part
        beam pointing left, and then skip a note. Same comment about the flags.
        */

        if (rightnotetype >= thisnotetype && right->spitch != 0 &&
          (right == beam_last || rightbreak2 ||
            ((misc_nextnote(right, NULL))->notetype < rightnotetype &&
              (rightmoff%lastlen != 0 ||
                (taildirflags[notenumber+1] & 3) == 2))))
          {
          int x = out_findXoffset(rightmoff) + adjusts[rightadjustptr] +
            sradjust + (right_up? beam_Xcorrection : 0);

          if (right_up != beam_upflag) Dlevel = throughlevel - level + 1;
          ps_beam(x - mac_muldiv(curmovt->beamflaglength, main_stavemagn, 1000),
            x, Dlevel, 0);
          taildirflags[notenumber+1] |= 2;
          skipnote = TRUE;
          }

        leftdone = FALSE;  /* New left note has no leftwards beam(let) */
        }

      /* Skip a note if flag has been set to do so or if secondary break */

      if ((skipnote || rightbreak2) && right != beam_last)
        {
        left = right;
        leftmoff = rightmoff;
        right = misc_nextnote(right, NULL);
        rightmoff += left->length;
        rightadjustptr++;
        while ((right->spitch == 0 && right != beam_last) ||
               (right->length == 0 && !out_gracenotes))
          {
          rightmoff += right->length;
          right = misc_nextnote(right, NULL);
          rightadjustptr++;
          }
        leftdone = FALSE;
        leftextended = rightextended;
        rightextended = FALSE;
        notenumber++;
        }
      }

    /* Advance left to end of group and loop for next pair */

    left = right;
    leftextended = rightextended;
    rightextended = FALSE;
    leftmoff = rightmoff;
    adjustptr = rightadjustptr;
    notenumber++;

    /* If we have had a break after the first note of the beam, set up as
    though just done a pair, in case it's a 2-note beam (weird notation!), and
    then clear the flag. */

    if (firstbreak2)
      {
      rightbreak2 = TRUE;
      firstbreak2 = FALSE;
      }
    }

  /* If there was a beam break immediately before the last note of the beamed
  group, then we need to draw a short beam pointing to the left on the last
  note. This is an unusual special case; normally beam breaks occur in the
  middle of groups. It can also occur at the start of a stave on a continued
  beam that has only one note in the new bar. Check for a rest, as this can
  happen when a beam over a barline occurs at the end of a system and the last
  thing in the first bar is a rest. Can also happen when beamendrests is set.
  */

  if (rightbreak2 && left->notetype >= thisnotetype && left->spitch != 0)
    {
    int x = out_findXoffset(leftmoff) + adjusts[adjustptr] + sradjust +
      ((beam_upflag && !lastopposite)? beam_Xcorrection: 0);
    ps_beam(x - mac_muldiv(curmovt->beamflaglength, main_stavemagn, 1000), x,
      level, 0);
    }

  /* Move on to next note type and loop, unless seen no shorter notes */

  thisnotetype++;
  lastlen /= 2;
  if (!donesomething) break;
  }
}



/*************************************************
*            Set up Beaming                      *
*************************************************/

/* This function is called at the start of a potential sequence of beamed
notes. It must test whether beaming is possible, and if so, output the beams
and set the beam line parameters for adjusting the tails of the actual notes.
Various other parameters are also set up.

This function is also called during paginating, from page_setcont(), when it
discovers the flag for beams crossing bar lines at the end of a system. The
purpose of this is to determine the slope for the beam so that it can be set up
in the bar_cont->overbeam structure for the start of the next system. Various
global variables are fudged up when this happens, and the "nodraw" flag is set
true.

Arguments:
  p             point to the start of the beam
  moff          the musical offset
  nodraw        see comment above (if TRUE, don't draw)
  restatstart   TRUE if there is a rest at the start of the beam

Returns:        TRUE if beaming is set up; FALSE otherwise.
*/

BOOL
out_setupbeam(b_notestr *p, int moff, BOOL nodraw, BOOL restatstart)
{
b_notestr *pp = p + 1;
b_notestr *lastp = p;
b_notestr *lastinbar = NULL;

int overbarcount = 0;
int type = pp->type;
int currentmoff = moff;

int longestnote = p->notetype;    /* a secondary beam break forces this */
int i, xright, yright;

BOOL lastopposite;
BOOL rightbreak2 = FALSE;

int adjust = 0;
int lastadjust = 0;
int lastmoff = 0;
int lastmoffinbar = 0;
int pendingrests = 0;
int trailingrests = 0;
int adjustptr = 1;
int adjusts[50];

DEBUG(("out_setupbeam() start\n"));

adjusts[0] = out_Xadjustment;

beam_count = 1;
beam_first = beam_last = p;
beam_firstmoff = beam_lastmoff = moff;
beam_overbeam = FALSE;

/* The stem direction is determined when the notes are read in. */

beam_upflag = n_upflag;

/* Scan to find end of beam. We have to use gotos to jump out of the loop from
within the switch. */

for (;;)
  {
  int included;

  switch (type)
    {
    case b_End:
    if (!((b_Endstr *)pp)->overbeam || beam_overbeam) goto ENDBEAM;

    /* The bar line has been flagged for continuation of a beam over it. If a
    subsequent bar exists, its data will follow on immediately after this
    one's, so we can just allow the continuation to happen, after noting the
    fact. */

    if (out_bar < curmovt->barcount)
      {
      stavestr *ss = curmovt->stavetable[out_stave];
      if (ss == NULL || ss->barindex[out_bar+1] == NULL) goto ENDBEAM;
      beam_overbeam = TRUE;
      overbarcount = adjust = 0;
      lastinbar = lastp;            /* May point to a rest */
      lastmoffinbar = lastmoff;
      }
    break;

    case b_Jump:
    pp = (b_notestr *)(((b_Jumpstr *)pp)->next);
    break;

    case b_note:
    included = TRUE;
    if (out_gracenotes)
      {
      if (pp->length != 0) goto ENDBEAM;
      }
    else
      {
      if (pp->length == 0) included = FALSE;
      }

    if (included)
      {
      int notetype = pp->notetype;
      if (notetype < quaver) goto ENDBEAM;
      if (lastp->length != 0)
        currentmoff += lastp->length; else currentmoff++;
      adjusts[adjustptr] = lastadjust = adjust + out_grace_fudge;

      if (pp->spitch != 0 || curmovt->beamendrests)
        {
        beam_lastmoff = currentmoff;
        beam_last = pp;
        if (notetype < longestnote) longestnote = notetype; /* for full beams */
        if (pp->spitch == 0) trailingrests++;
          else trailingrests = 0;
        pendingrests = 0;
        }
      else pendingrests++;

      beam_count++;
      overbarcount++;
      lastp = pp;
      lastmoff = currentmoff;
      }

    adjustptr++;
    adjust = 0;
    break;

    case b_beambreak:
    case b_reset:
    goto ENDBEAM;

    case b_beambreak2:
      {
      int j = quaver - 1 + ((b_beambreak2str *)pp)->value;
      if (j < longestnote) longestnote = j;
      }
    break;

    /* Set up move amount in case the next thing is a note. We need to keep
    track of movements to get the beams moved too. */

    case b_move:
    adjust = ((b_movestr *)pp)->x;
    break;

    /* Miscellaneous things that are permitted in the middle of beams. If it's
    a printing thing, cancel any pending move, since that will apply to the
    thing, not the following note. */

    case b_dotbar: case b_clef: case b_lrepeat: case b_rrepeat:
    case b_comma: case b_caesura: case b_tick:
    adjust = 0;
    break;
    }

  /* Move on to the next item */

  pp = (b_notestr *)((uschar *)pp + length_table[type]);
  type = pp->type;
  }

/* Label to jump to to get out of the switch in the loop */

ENDBEAM:

/* Correct the count for rests that might have been beamed over, but weren't.
*/

beam_count -= pendingrests;

/* There must be at least two notes, except in the case of a continued beam at
the start of a system. In this case, a forced slope will be set. */

if (beam_last == beam_first && !beam_continued)
  {
  DEBUG(("out_setupbeam() FALSE\n"));
  return FALSE;
  }

/* Catch the case where beaming over the barline wasn't. If it's the paginating
case, just return. */

if (beam_overbeam && overbarcount <= 0)
  {
  if (nodraw)
    {
    DEBUG(("out_setupbeam() FALSE\n"));
    return FALSE;
    }
  beam_overbeam = FALSE;
  }

/* Note if the last note's stem goes the opposite way. A rest must be treated
as an upstem so that the beam gets drawn past it. */

if (beam_last->spitch == 0) lastopposite = !beam_upflag;
  else lastopposite = (((beam_last->flags) & nf_stemup) != 0) != beam_upflag;

/* Compute the xcorrection for any notes with stems up, and for notes on the
opposite side of the beam. */

beam_Xcorrection = mac_muldiv(51*main_stavemagn, n_fontsize, 100000);

/* Set up the coordinates of the left-hand end of the beam, relative to the
stave base and the start of the bar (with the y coordinate going upwards). The
x correction of 0.75 point is required because the downward stems start that
far from the note's origin. */

beam_firstX = out_findXoffset(beam_firstmoff) + adjusts[0] +
  mac_muldiv(n_fontsize, 75*main_stavemagn, 1000000);

beam_firstpitch = p->spitch;

if (beam_upflag && !restatstart) beam_firstX += beam_Xcorrection;
mac_couplepitch(beam_firstpitch, p->flags);

beam_firstY = (beam_firstpitch - 128 + n_upfactor*14)*main_stavemagn;

/* This "loop", which computes a slope and an offset, is obeyed twice if on the
first pass we fail to arrange for notes on the opposite side of the beam, and
therefore have to do it all again. Exit should always be via "break", but code
it with a counter, just in case. */

for (i = 0; i < 2; i++)
  {
  int j, pitchright;
  BOOL offsetOK = FALSE;

  /* Set up coordinates of end notes and default parameters. The 0.115 point
  correction is the 0.75 pt offset to the left of a downward stem, plus the
  0.4pt thickness of stems. */

  xright = out_findXoffset(beam_lastmoff) + lastadjust +
    mac_muldiv(n_fontsize, 115*main_stavemagn, 1000000);

  if (beam_upflag != lastopposite) xright += beam_Xcorrection;

  pitchright = beam_last->spitch;
  mac_couplepitch(pitchright, beam_last->flags);
  yright = (pitchright - 128 +
    (((beam_last->flags & nf_stemup) != 0)? 14 : -14))*main_stavemagn;

  /* Now we must select a slope for the beam. First check to see if something
  has been forced. */

  if (beam_forceslope != BIGNUMBER) beam_slope = beam_forceslope;

  /* If there is only one real note in the beam (can happen with leading and/or
  trailing rests) the slope is horizontal. */

  else if (beam_count - trailingrests < 2) beam_slope = 0;

  /* If there are only two notes in the beam, we are just going to join the
  ends of their stems. If the stems are in the same direction, calculate a
  slope without taking into account any stem extensions. Then an extension on
  either note moves the beam outwards, without changing its slope. For stems in
  opposite directions, however, we must take note of the stem extensions, and
  further fudge the right-hand y coordinate because the beam offset will be
  taken from the left-hand note's stem extension.

  For non-opposite notes whose stems stick out above or below the stave, we
  shorten the stems slightly if they are not already so shortened. */

  else if (beam_count == 2)
    {
    int yyright = yright;
    int yyleft = beam_firstY;

    if (lastopposite)
      {
      int yleftadjust = beam_first->yextra;
      int yrightadjust = beam_last->yextra;
      yyleft += n_upfactor*yleftadjust;
      yyright -= n_upfactor*yrightadjust;
      yright -= n_upfactor*(yleftadjust + yrightadjust);
      }

    beam_slope = mac_muldiv(1000, yyright-yyleft, xright-beam_firstX);

    /* Apply a maximum slope */

    if (abs(beam_slope) > curmovt->maxbeamslope1)
      beam_slope = (beam_slope/abs(beam_slope))*curmovt->maxbeamslope1;
    }

  /* More than two notes in the beam. Select a slope using the heuristics in
  the findslope() routine with some additional constraints. */

  else
    {
    int abs_slope;
    int slope = findslope();
    int yslope = beam_firstY + mac_muldiv(slope, xright-beam_firstX, 1000);

    /* If the slope supplied by the heuristic is zero, or if it is less than
    the slope of the line joining the end notes, use it. Otherwise join the end
    notes. (Not strictly true for split-sided beams.) */

    if (slope == 0 || (slope > 0 && yslope < yright) ||
      (slope < 0 && yslope > yright)) yright = yslope;

    beam_slope = mac_muldiv(1000, yright-beam_firstY, xright-beam_firstX);

    /* When the final note is on the other side of the beam, it is helpful to
    keep the slope down to a smaller maximum to enable more cases to be fitted
    in without retries. */

    abs_slope = abs(beam_slope);
    if (lastopposite && abs_slope > 200)
      beam_slope = mac_muldiv(beam_slope, 200, abs_slope);
    }

  /* Inner loop for retrying with a smaller maximum slope when we fail to find
  an offset for a beam with notes on either side of it. For ordinary beams, the
  loop will be broken on the first pass. */

  for (j = 0; j < 5; j++)
    {
    int ornament = (n_ornament == NULL)? -1 : n_ornament->ornament;
    if ((offsetOK =
      ComputeBeamOffset(ornament, lastopposite, adjusts, longestnote))
        == TRUE) break;
    beam_slope = n_upfactor*retryslopes[j];
    }

  /* Break out of the outer loop if we have successfully found an offset.
  Otherwise, we have to put all notes on the same side of the beam, and give an
  error message. Then try again -- success is now assured! */

  if (offsetOK) break; else
    {
    b_notestr *bfp = beam_first;
    usint flag = beam_upflag? nf_stemup : 0;

    for (;;)
      {
      if ((bfp->flags & nf_stemup) != flag) read_resetstemflag(bfp, flag);
      if (bfp == beam_last) break;
      bfp = misc_nextnote(bfp, NULL);
      }

    error_moan(ERR61, out_bar, out_stave);
    lastopposite = FALSE;
    }
  }

/* Reset the forced slope */

beam_forceslope = BIGNUMBER;

/* Incorporate the computed and manual adjustment offset into the first y
position. */

if (beam_upflag)
  {
  if (beam_offsetadjust < -minoffset) beam_offsetadjust = -minoffset;
  }
else if (beam_offsetadjust > minoffset) beam_offsetadjust = minoffset;

beam_firstY += beam_offset + beam_offsetadjust;
beam_offsetadjust = 0;

/* If beam_continued is set, it means we are drawing a continued beam at the
start of a new system. We need to extend it a little bit to the left. Do this
by adjusting firstX and firstY. Also set the longestnote value for the whole
beam, to control how many beams are extended left, and set rightbreak2 so that
if there is only one note, and it is shorter than the main beams, a stub to the
left is drawn. */

if (beam_continued)
  {
  int beamadjust = 4 * main_stavemagn;
  if (beam_upflag) beamadjust += beam_Xcorrection;
  beam_firstX -= beamadjust;
  beam_firstY -= (beam_slope * beamadjust) / 1000;

  longestnote = (bar_cont->overbeam)->longestnote;
  if (beam_count == 1) rightbreak2 = TRUE;
  }

/* Now we can draw the beams, adjusting for grace or cue notes. We don't draw
in the case of being called while paginating, while if we are at the end of a
line and drawing a continued beam, we must chop it. */

if (!nodraw)
  {
  if (beam_overbeam && out_lineendflag)
    {
    beam_lastmoff = lastmoffinbar;
    beam_last = lastinbar;
    xright = out_barlinex - 300 - out_barx;
    }
  DrawBeams(xright, adjusts, longestnote, lastopposite, rightbreak2);
  }

/* If this beam extends over the bar line, set up the data for the start of the
next bar, and cancel the special flag, because it affects the behaviour of
various subroutines. */

if (beam_overbeam)
  {
  obeamstr *b;
  if (bar_cont->overbeam == NULL)
    bar_cont->overbeam = store_Xget(sizeof(obeamstr));
  b = bar_cont->overbeam;

  b->firstX = beam_firstX - out_poslast->xoff - out_sysblock->barlinewidth;
  b->firstY = beam_firstY;
  b->slope = beam_slope;
  b->count = overbarcount;
  b->longestnote = longestnote;
  b->Xcorrection = beam_Xcorrection;
  b->splitOK = beam_splitOK;
  b->upflag = beam_upflag;

  beam_overbeam = FALSE;
  }

/* A yield of TRUE indicates that beaming is now in force */

DEBUG(("out_setupbeam() TRUE\n"));
return TRUE;
}

/* End of setbeam.c */
