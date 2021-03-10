/*************************************************
*    The PMW Music Typesetter - 3rd incarnation  *
*************************************************/

/* Copyright (c) Philip Hazel, 1991 - 2021 */

/* Written by Philip Hazel, starting November 1991 */
/* This file last modified: January 2021 */


/* This file contains all the global data */


#include "pmwhdr.h"
#include "readhdr.h"
#include "outhdr.h"
#include "pagehdr.h"
#include "poshdr.h"


bstr    **barvector;

movtstr *format_movt;
movtstr **movement;
movtstr *curmovt;
pagestr *curpage;

int      curstave;

FILE *input_file = NULL;
FILE *debug_file = NULL;
BOOL debugging = FALSE;

filestr *read_filestack;
int      read_filestackptr;

uschar *arg_from_name = NULL;
uschar *arg_to_name = NULL;

uschar *baraccs = NULL;
uschar *baraccs_tp = NULL;

contstr *bar_cont;

b_notestr *beam_first;
b_notestr *beam_last;

uschar *beam_stemadjusts;

BOOL  bar_use_draw = FALSE;
int   beam_accrit;
BOOL  beam_continued;
int   beam_count;
int   beam_firstmoff;
int   beam_firstpitch;
int   beam_firstX;
int   beam_firstY;
int   beam_forceslope;
int   beam_lastmoff;
int   beam_offset;
int   beam_offsetadjust;
int   beam_overbeam;
int   beam_seq;
int   beam_slope;
int   beam_splitOK;
int   beam_upflag;
int   beam_Xcorrection;

uschar *copyright;

tree_node *define_tree;
tree_node *draw_tree;
tree_node *draw_variable_tree;

int draw_gap = 0;
int draw_lgx;
int draw_lgy;
int draw_nextvariable;
int draw_ox;
int draw_oy;
int draw_thickness;

int  error_count;
int  error_maximum = 40;
int  error_ptr_correction = 0;
int  error_skip;
BOOL error_111 = FALSE;

#ifdef SUPPORT_B2PF
b2pf_context **font_b2pf_contexts;
uint32_t *font_b2pf_options;
#endif
int  font_basecount = 0;
int  font_count;
fontstr *font_List;
uschar *font_data_default;
uschar *font_data_extra;
uschar *font_music_default;
uschar *font_music_extra;
int  font_sinr;
int  font_cosr;
int  font_stringheight;
int  *font_table;
int  font_transform[6];
int  font_xstretch;

uschar *main_filename;
int  main_firstpage;
int  *main_fonttable;
uschar *main_format = US"";
BOOL main_format_tested;
htypestr *main_htypes;
int  main_initialized = FALSE;
BOOL main_kerning;
keytransposestr *main_keytranspose = NULL;
int  main_lastmovement = 0;
int  main_lastpage;
int  main_magnification;
int  main_max_bars = DEF_MAX_BARS;
int  main_max_movements = DEF_MAX_MOVEMENTS;
int  main_maxvertjustify;
uschar *main_musicchoice = US"PMW-Music";
int  main_notenum;
int  main_noteden;
int  main_notespacing[8];
pagestr *main_pageanchor;
int  main_pageinc;
int  main_pagelength;
pkeystr *main_printkey;
ptimestr *main_printtime;
headstr *main_pssetup;
int  main_rc = 0;
BOOL main_righttoleft = FALSE;
int  main_sheetheight;
int  main_sheetwidth;
BOOL main_shownlogo = FALSE;
int  main_stavemagn;
usint  main_staves[STAVE_BITVEC_SIZE] = { -1, -1 };
int  main_storetotal = 0;
int  main_storestaves = 0;
int  main_stretchrespacethresh = 1075;
int  main_stretchthreshnum = 1;
int  main_stretchthreshden = 2;
int  main_totalbars;
int  main_tracepos = (-2);
int  main_transpose = no_transpose;
BOOL main_transposedaccforce;
trkeystr *main_transposedkeys;
int  main_truepagelength;
uschar main_xkeys[MAX_XKEYS][10];
uschar main_xkeyorder[MAX_XKEYS][10];

uschar *midi_filename = NULL;
BOOL    midi_for_notes_off = FALSE;
uschar *midi_perc = NULL;
uschar *midi_percnames = NULL;
BOOL    midi_poly = TRUE;
uschar *midi_voices = NULL;
uschar *midi_voicenames = NULL;

int  n_acc;
int  n_accleft;
BOOL n_beamed;
int  n_cueadjust;
int  n_chordcount;
int  n_chordflags;
usint n_chordacflags;
int  n_dotxadjust;
int  n_firstacc;
int  n_flags;
usint n_acflags;
int  n_fontsize;
int  n_gracecount;
int  n_gracemoff;
BOOL n_nhtied;
BOOL n_invertleft;
BOOL n_invertright;
int  n_lastacc;
b_notestr *n_lastnote;
int  n_length;
int  n_longrestmid;
int  n_masq;
int  n_maxaccleft;
int  n_maxpitch;
int  n_minpitch;
b_tiestr *n_nexttie;
int  n_notetype;
int  n_orig_stemlength;
b_ornamentstr *n_ornament;
int  n_pcorrection;
int  n_pitch;
b_tiestr *n_prevtie;
int  n_restlevel;
int  n_stemlength;
int  n_upflag;
int  n_upfactor;
int  n_x;

BOOL opt_landscape;
BOOL opt_oldbeambreak;
BOOL opt_oldrestlevel;
BOOL opt_oldstemlength;
BOOL opt_print_postscript = FALSE;
int  opt_sheetsize = sheet_unknown;
int  opt_stretchrule;

int  out_bar;
int  out_barchar;
int  out_barlinex;
int  out_barx;
int  out_bbox[4];
int  out_beaming;
contstr *out_cont;
int  out_dashlength;
int  out_dashgaplength;
int  out_deepybarend;
int *out_depthvector;
int  out_downgap;
b_drawstr *out_drawqueue[20];
int  out_drawqueueptr;
int  out_drawstackptr;
int  out_dynmovef[dyn_max];
int  out_dynmovex[dyn_max];
int  out_dynmovey[dyn_max];
BOOL out_gracefound;
int  out_grace_fudge;
BOOL out_gracenotes;
BOOL out_hairpinhalf;
int  out_joinxposition;
int  out_keycount;
int  out_lastbarlinex;
BOOL out_lastbarwide;
int  out_lastmoff;
BOOL out_lastnotebeamed;
int  out_lastnotex;
int  out_laststave;
uschar out_laststemup[MAX_STAVE+1];
BOOL out_lineendflag;
BOOL out_makedraw = FALSE;
int  out_manyrest;
int  out_moff;
b_notestr *out_notelist[20];
int  out_notex;
BOOL out_omitbarline;
overdrawstr *out_overdraw;
BOOL out_passedreset;
int  out_pden;
b_pletstr *out_plet;
int  out_plet_highest;
int  out_plet_highest_head;
int  out_plet_lowest;
int  out_plet_x;
int  out_pnum;
posstr *out_poslast;
posstr *out_posptr;
posstr *out_postable;
int  out_posxRL;
b_prevbarstr *out_prevbar;
int  out_prevtieflag;
BOOL out_repeatonbarline;
int  out_slurclx = 0;
int  out_slurcly = 0;
int  out_slurcrx = 0;
int  out_slurcry = 0;
BOOL out_slurstarted;
BOOL out_startlinebar;
int  out_stave;
int  out_stavelines;
int  out_stavebottom;
int  out_stavetop;
int  out_string_endx;
int  out_string_endy;
sysblock *out_sysblock;
int  out_textnextabove;
int  out_textnextbelow;
b_textstr *out_textqueue[TEXT_QUEUE_SIZE];
b_textXstr *out_textXqueue[TEXT_QUEUE_SIZE];
int  out_textqueueptr;
b_textXstr *out_textX;
int  out_timecount;
b_tremolostr *out_tremolo;
BOOL out_tremupflag;
int  out_tremx;
int  out_tremy;
int  out_upgap;
int  out_ybarend;
int  out_Xadjustment;
int  out_Yadjustment;
int  out_zcopycount;

int  out_yposition;
int  out_ystave;

int  output_copies = 1;
BOOL output_duplex = FALSE;
BOOL output_incPMWfont = FALSE;
BOOL output_manualfeed = FALSE;
stave_list *output_pagelist = NULL;
BOOL output_tumble = FALSE;

pagedatastr *page_accepteddata;
pagedatastr *page_nextdata;
pagedatastr *page_previousdata;

int  page_barcount;
int  page_barlinewidth;
int  page_barnumber;
int  page_botmargin;
contstr *page_cont;
int  page_countsystems;
BOOL page_firstsystem;
headstr *page_footing;
int  page_footnotedepth;
headstr *page_footnotes;
int  page_footnotespacing;
int  page_justify;
int  page_justifyLR;
int  page_lastbarcountbump;
BOOL page_lastendwide;
BOOL page_lastenddouble;
headstr *page_lastfootnote;
headstr *page_lastnewfootnote;
sysblock *page_lastsystem;
int  page_lastulevel;
int  page_lastwanted;
int  page_layout_stretchd;
int  page_layout_stretchn;
int  page_manyrest;
int  page_movtnumber;
BOOL page_movtpending;
int  page_newfootnotedepth;
headstr *page_newfootnotes;
BOOL page_newpagewanted;
int  page_olaysize;
int *page_olevel;
int *page_olhere;
workposstr *page_posptr;
workposstr *page_postable;
BOOL page_savehadmovt;
int  page_sgnext;
usint page_showtimes[STAVE_BITVEC_SIZE];
int *page_ssnext;
int *page_ssehere;
int *page_ssenext;
BOOL page_startchangetime;
startlinestr *page_startline;
BOOL page_startlinebar;
int  page_stave;
usint  page_stavemap[STAVE_BITVEC_SIZE];
uschar *page_sysclef;
sysblock *page_sysblock;
sysblock **page_sysprevptr;
int  page_sys_botmargin;
int  page_sys_justify;
int  page_sys_topmargin;
int  page_topmargin;
int  page_ulaysize;
int *page_ulevel;
int *page_ulhere;
BOOL page_warnkey;
BOOL page_warntime;
int  page_xxwidth;

int  play_endbar = -1;
int  play_movt_number = -1;
BOOL play_repeats = TRUE;
int  play_startbar = -1;

barposstr *pos_bp;
BOOL pos_barstartrepeat;

stave_list *print_curlist;
int  print_curnumber;
int  print_gutter = 0;
int  print_imposition = -1;
int  print_lastpage;
int  print_image_xadjust = 0;
int  print_image_yadjust = 0;
int  print_image_sxadjust = 0;
int  print_image_syadjust = 0;
int  print_magnification = 1000;
int  print_pagefeed = -1;
int  print_pageorigin;
BOOL print_pamphlet = FALSE;
BOOL print_reverse = FALSE;
BOOL print_side1 = TRUE;
BOOL print_side2 = TRUE;

int  print_xpageoffset = 72000;
int  print_ypageoffset = 53000;

FILE *ps_file;
uschar *ps_header = NULL;

BOOL reading_input = FALSE;
int  read_barlinestyle;
int  read_ch;
uschar *read_chptr;
BOOL read_copied_fontsizestr;
dirstr *read_dir;
uschar *read_endptr;
BOOL read_endstave;
BOOL read_EOF;
int  read_headcount;
int  read_headmap;
b_ensurestr *read_lastensuredtie;
b_playchangestr **read_lastplaychange;
int  read_linenumber;
int  read_okdepth;
int  read_prev_had_dbar;
int  read_prev_had_ibar;
int  read_prev_barlinestyle;
int  read_skipdepth;
BOOL read_stavedir = FALSE;
uschar *read_word;

stavestr *stavehead;
stavestr **stavetable;

usint stave_accentflags;
int  stave_accritvalue;
int  stave_barlength;
int  stave_barlinestyle;
int  stave_barnumber;
int  stave_barrepeatcount;
bstr *stave_barrepeatptr;
int  stave_beamcount;
b_notestr *stave_beamfirstnote;
BOOL stave_beaming;
b_notestr **stave_beamstack;
int  stave_beamstackptr;
int  stave_beamstemforce;
BOOL stave_checklength;
int  stave_chordcount;
int  stave_clef;
int  stave_clef_octave;
BOOL stave_copy_accs;
int  stave_couplestate;
int  stave_fbfont;
int  stave_fbsize;
BOOL stave_firstinbar;
b_notestr *stave_firstnoteptr;
BOOL stave_hadnocount;
int  stave_hairpinbegun;
int  stave_hairpinflags;
int  stave_hairpinsru;
int  stave_hairpinwidth;
int  stave_hairpiny;
int  stave_key;
int  stave_key_tp;
int  stave_lastgracestem;
b_notestr *stave_lastbasenoteptr;
b_notestr *stave_lastnoteptr;
BOOL stave_laststemup;
int  stave_lasttiepitch;
BOOL stave_lastwasdouble;
BOOL stave_lastwastied;
int  stave_matchnum;
int  stave_matchden;
int  stave_maxaway;
int  stave_maxpitch;
int  stave_minpitch;
BOOL stave_notes;
int  stave_noteden;
int  stave_notenum;
int  stave_octave;
int  stave_olfont;
int  stave_olsize;
int  stave_ornament;
BOOL stave_overbeam;
ulaypend *stave_pendulay;
int  stave_pitchcount;
int  stave_pitchtotal;
int  stave_pletflags;
int  stave_pletlen;
int  stave_pletsupnum;
int  stave_pletsupden;
int  stave_plety;
int  stave_printpitch;
int  stave_requiredbarlength;
BOOL stave_resetOK;
int  stave_restlevel;
int  stave_maxbarlength;
int  stave_noteflags;
int  stave_slurcount;
int  stave_smove;
BOOL stave_smove_relative;
int  stave_stemflag;
int  stave_stemforce;
int  stave_stemlength;
b_notestr **stave_stemstack;
int  stave_stemstackptr;
int *stave_stemswaplevel;
BOOL stave_string_followOK;
BOOL stave_suspended;
int  stave_textabsolute;
int  stave_textflags;
int  stave_textfont;
int  stave_textsize;
tiedata *stave_tiedata;
int  stave_ties;
int  stave_totalnocount;
int  stave_transpose;
int  stave_transpose_letter;
BOOL stave_transpose_letter_is_auto;
BOOL stave_transposedaccforce;
BOOL stave_tripletize;
int  stave_ulfont;
int  stave_ulsize;
int  stave_use_draw = 0;
BOOL stave_use_widechars = TRUE;

int  string_font;

uschar *next_buffer;
uschar *this_buffer;
uschar *prev_buffer;

BOOL verify;
int  version_fixed;
uschar version_string[40];

/* End of globals.c */
