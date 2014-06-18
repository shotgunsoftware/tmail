/*

    tmailscanner.c

    Copyright (c) 1998-2007 Minero Aoki

    This program is free software.
    You can distribute/modify this program under the terms of
    the GNU Lesser General Public License version 2.1.

*/

#include <stdio.h>
#ifdef __STDC__
# include <stdlib.h>
#endif


#include "ruby.h"
#ifndef RSTRING_PTR
#define RSTRING_PTR(obj) RSTRING(obj)->ptr
#endif
 
#ifndef RSTRING_LEN
#define RSTRING_LEN(obj) RSTRING(obj)->len
#endif

#ifdef HAVE_RUBY_RE_H
#include "ruby/re.h"
#else
#include "re.h"
#endif

#ifdef HAVE_RUBY_ENCODING_H
#include "ruby/encoding.h"
#define ismbchar(c) (!rb_enc_isascii((unsigned char)(c),NULL))
#endif

#define TMAIL_VERSION "1.2.3"

static VALUE TMailScanner;
static VALUE ScanError;

struct scanner
{
    char *pbeg;
    char *p;
    char *pend;
    unsigned int flags;
    VALUE comments;
#ifdef HAVE_RUBY_ENCODING_H
    rb_encoding *enc;
#endif
};

#define MODE_MIME     (1 << 0)
#define MODE_RECV     (1 << 1)
#define MODE_ISO2022  (1 << 2)
#define MODE_DEBUG    (1 << 4)

#define MIME_MODE_P(s)     ((s)->flags & MODE_MIME)
#define RECV_MODE_P(s)     ((s)->flags & MODE_RECV)
#define ISO2022_MODE_P(s)  ((s)->flags & MODE_ISO2022)

#ifndef HAVE_RB_DATA_TYPE_T_FUNCTION
#define GET_SCANNER(val, s) Data_Get_Struct(val, struct scanner, s)
#define MAKE_SCANNER(klass, s) Data_Make_Struct(klass, struct scanner, 0, -1, sc)
#else
#define GET_SCANNER(val, s) TypedData_Get_Struct(val, struct scanner, &mails_type, s)
#define MAKE_SCANNER(klass, s) TypedData_Make_Struct(klass, struct scanner, &mails_type, s)


static void
mails_mark(ptr)
    void *ptr;
{
    struct scanner *sc = ptr;
    rb_gc_mark(sc->comments);
}

static size_t
mails_memsize(ptr)
    const void *ptr;
{
    return ptr ? sizeof(struct scanner) : 0;
}

static const rb_data_type_t mails_type = {
    "TMailScanner",
    {mails_mark, RUBY_TYPED_DEFAULT_FREE, mails_memsize,},
};
#endif

#ifndef StringValue
#  define StringValue(s) Check_Type(str, T_STRING);
#endif

static int
is_japanese(str)
    VALUE str;
{
#ifdef HAVE_RUBY_ENCODING_H
    rb_encoding *enc = rb_enc_get(str);
    const char *name = rb_enc_name(enc);
    return strcmp(name, "ISO-2022-JP") == 0;
#else
    const char *tmp = rb_get_kcode();
    return strcmp(tmp, "EUC") == 0 || strcmp(tmp, "SJIS") == 0;
#endif
}

/*
 * Document-method: mails_init
 *
 * Creates a new mail
 *
 */
static VALUE
mails_init(obj, str, ident, cmt)
    VALUE obj, str, ident, cmt;
{
    struct scanner *sc;
    const char *tmp;

    GET_SCANNER(obj, sc);

    StringValue(str);
    sc->pbeg = RSTRING_PTR(str);
    sc->p    = sc->pbeg;
    sc->pend = sc->p + RSTRING_LEN(str);
#ifdef HAVE_RUBY_ENCODING_H
    sc->enc = rb_enc_get(str);
#endif

    sc->flags = 0;
    Check_Type(ident, T_SYMBOL);
    tmp = rb_id2name(SYM2ID(ident));
    if      (strcmp(tmp, "RECEIVED")     == 0) sc->flags |= MODE_RECV;
    else if (strcmp(tmp, "CTYPE")        == 0) sc->flags |= MODE_MIME;
    else if (strcmp(tmp, "CENCODING")    == 0) sc->flags |= MODE_MIME;
    else if (strcmp(tmp, "CDISPOSITION") == 0) sc->flags |= MODE_MIME;

    if (is_japanese(str)) {
        sc->flags |= MODE_ISO2022;
    }

    sc->comments = Qnil;
    if (! NIL_P(cmt)) {
        Check_Type(cmt, T_ARRAY);
        sc->comments = cmt;
    }

    return obj;
}

static VALUE
mails_s_alloc(klass)
    VALUE klass;
{
    struct scanner *sc;
    return MAKE_SCANNER(klass, sc);
}

#ifndef HAVE_RB_DEFINE_ALLOC_FUNC
static VALUE
mails_s_new(klass, str, ident, cmt)
    VALUE klass, str, ident, cmt;
{
    return mails_init(mails_s_alloc(klass), str, ident, cmt);
}
#endif

/*
 * Document-method: mails_debug_get
 *
 * TODO: Documentation needed
 *
 */
static VALUE
mails_debug_get(self)
    VALUE self;
{
    struct scanner *sc;

    GET_SCANNER(self, sc);
    if (sc->flags & MODE_DEBUG)
        return Qtrue;
    else
        return Qfalse;
}

/*
 * Document-method: mails_debug_set
 *
 * TODO: Documentation needed
 *
 */
static VALUE
mails_debug_set(self, flag)
    VALUE self, flag;
{
    struct scanner *sc;

    GET_SCANNER(self, sc);
    if (RTEST(flag))
        sc->flags |= MODE_DEBUG;
    else
        sc->flags &= ~MODE_DEBUG;
    return Qnil;
}


/*
----------------------------------------------------------------------
                        scanning routines
----------------------------------------------------------------------
*/

#define ESC               '\033'
#define ATOM_SYMBOLS      "_#!$%&'`*+-{|}~^/=?"
#define TOKEN_SYMBOLS     "_#!$%&'`*+-{|}~^."
#define ATOM_SPECIAL      "()<>[]@,;:\"\\."
#define TOKEN_SPECIAL     "()<>[]@,;:\"\\/?="
#define LWSP              " \t\r\n"

#define IS_ALPHA(ch)      (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z'))
#define IS_UPPER(ch)      ('A' <= ch && ch <= 'Z')
#define TO_LOWER(ch)      (IS_UPPER(ch) ? ch + 32 : ch)
#define IS_LWSP(ch)       (strchr(LWSP, ch))
#define IS_DIGIT(ch)      ('0' <= ch && ch <= '9')
#define IS_WORDCHAR(ch, symlist) \
                          (IS_ALPHA(ch) || IS_DIGIT(ch) || strchr(symlist, ch))
#define IS_ATOMCHAR(ch)   IS_WORDCHAR(ch, ATOM_SYMBOLS)
#define IS_TOKENCHAR(ch)  IS_WORDCHAR(ch, TOKEN_SYMBOLS)
#define IS_JCHAR(ch)      ismbchar(ch)


/* I know this implement is ugly, but usually useful. */

/* skip until "\e(B" (us-ascii) */
static void
skip_iso2022jp_string(sc)
    struct scanner *sc;
{
    for (; sc->p < sc->pend; sc->p++) {
        if (*sc->p == ESC) {
            if (strncmp(sc->p, "\033(B", 3) == 0) {
                sc->p += 3;
                return;
            }
        }
    }
}

static void
skip_japanese_string(sc)
    struct scanner *sc;
{
    while (sc->p < sc->pend) {
#ifdef HAVE_RUBY_ENCODING_H
        if (! ismbchar(*sc->p)) return;
	sc->p += mbclen(sc->p, sc->pend, sc->enc);
#else
        if (! ismbchar(*sc->p)) return;
        sc->p += mbclen(*sc->p);
#endif
    }
}

#define scan_atom(sc) scan_word(sc, ATOM_SYMBOLS)
#define scan_token(sc) scan_word(sc, TOKEN_SYMBOLS)

static VALUE
scan_word(sc, syms)
    struct scanner *sc;
    const char *syms;
{
    const char *beg = sc->p;

    while (sc->p < sc->pend) {
        if (ISO2022_MODE_P(sc) && *sc->p == ESC) {
            skip_iso2022jp_string(sc);
        }
        else if (IS_JCHAR(*sc->p)) {
            skip_japanese_string(sc);
        }
        else if (IS_WORDCHAR(*sc->p, syms)) {
            sc->p++;
        }
        else {
            break;
        }
    }

    return rb_str_new(beg, sc->p - beg);
}


#define BUFSIZE 256

static VALUE
scan_quoted_word(sc)
    struct scanner *sc;
{
    char buf[BUFSIZE];
    char *p;
    char *save;
    VALUE result = rb_str_new("", 0);

    sc->p++;   /* discard first dquote */
    p = buf;
    while (sc->p < sc->pend) {
        if (*sc->p == '"') {
            sc->p++;   /* discard terminal dquote */
            rb_str_cat(result, buf, p - buf);
            return result;
        }
        if (ISO2022_MODE_P(sc) && *sc->p == ESC) {
            save = sc->p;
            skip_iso2022jp_string(sc);
            while (save < sc->p) {
                *p++ = *save++;
                if (p >= buf + BUFSIZE) {
                    /* flush buffer */
                    rb_str_cat(result, buf, BUFSIZE);
                    p = buf;
                }
            }
            continue;
        }

        if (*sc->p == '\\')
            sc->p++;   /* discard quoting backslash */
        *p++ = *sc->p++;
        if (p >= buf + BUFSIZE) {
            /* flush buffer */
            rb_str_cat(result, buf, BUFSIZE);
            p = buf;
        }
    }

    rb_raise(ScanError, "unterminated quoted-word");
    return Qnil;
}

static VALUE
scan_domain_literal(sc)
    struct scanner *sc;
{
    char buf[BUFSIZE];
    char *p;
    VALUE result = rb_str_new("", 0);

    p = buf;
    while (sc->p < sc->pend) {
        if (*sc->p == ']') {
            *p++ = *sc->p++;
            rb_str_cat(result, buf, p - buf);
            return result;
        }

        if (*sc->p == '\\')
            sc->p++;   /* discard backslash */
        *p++ = *sc->p++;
        if (p >= buf + BUFSIZE) {
            /* flush buffer */
            rb_str_cat(result, buf, BUFSIZE);
            p = buf;
        }
    }

    rb_raise(ScanError, "unterminated domain literal");
    return Qnil;
}


static VALUE
scan_comment(sc)
    struct scanner *sc;
{
    int nest = 1;
    char *p;
    VALUE ret = rb_str_new("", 0);

    sc->p++;
    p = sc->p;
    while (sc->p < sc->pend) {
        if (ISO2022_MODE_P(sc) && *sc->p == ESC) {
            skip_iso2022jp_string(sc);
        }
        else if (IS_JCHAR(*sc->p)) {
            skip_japanese_string(sc);
        }
        else {
            switch (*sc->p) {
            case '(':
                nest++;
                break;
            case ')':
                nest--;
                if (nest == 0) {
                    rb_str_cat(ret, p, sc->p - p);
                    sc->p++;
                    return ret;
                }
                break;
            case '\\':
                rb_str_cat(ret, p, sc->p - p);
                sc->p++;
                if (sc->p == sc->pend)
                    rb_raise(ScanError, "incomplete char quote");
                p = sc->p;
                break;
            default:
                break;
            }
            sc->p++;
        }
    }

    rb_raise(ScanError, "unterminated comment");
    return Qnil;
}


static void
skip_lwsp(sc)
    struct scanner *sc;
{
    while (sc->p < sc->pend) {
        if (IS_LWSP(*sc->p)) sc->p++;
        else break;
    }
}

static int
nccmp(a, b)
    const char *a, *b;
{
    while (*a && *b) {
        if ((*a != *b) && (TO_LOWER(*a) != TO_LOWER(*b)))
            return 0;
        a++; b++;
    }
    return (*a == *b);
}

static int
digit_p(str)
    VALUE str;
{
    char *p;
    int i;

    p = RSTRING_PTR(str);
    for (i = 0; i < RSTRING_LEN(str); i++) {
        if (! IS_DIGIT(RSTRING_PTR(str)[i]))
            return 0;
    }
    return 1;
}

static VALUE tok_atom, tok_digit, tok_token, tok_quoted, tok_domlit;
static VALUE tok_from, tok_by, tok_via, tok_with, tok_id, tok_for;

static VALUE
atomsym(sc, str)
    struct scanner *sc;
    VALUE str;
{
    if (digit_p(str)) {
        return tok_digit;
    }
    else if (RECV_MODE_P(sc)) {
        char *p = RSTRING_PTR(str);
        if      (nccmp(p, "from")) return tok_from;
        else if (nccmp(p, "by"))   return tok_by;
        else if (nccmp(p, "via"))  return tok_via;
        else if (nccmp(p, "with")) return tok_with;
        else if (nccmp(p, "id"))   return tok_id;
        else if (nccmp(p, "for"))  return tok_for;
    }
    return tok_atom;
}

static void
debug_print(sc, sym, val)
    struct scanner *sc;
    VALUE sym, val;
{
    VALUE s;

    s = rb_funcall(sym, rb_intern("inspect"), 0),
    printf("%7ld %-10s token=<%s>\n",
           (unsigned long)(sc->pend - sc->p),
           RSTRING_PTR(s),
           RSTRING_PTR(val));
}

#define D(expr) do {\
    if (sc->flags & MODE_DEBUG) {expr;}\
} while (0)

static void
pass_token(sc, sym, tok, arr)
    struct scanner *sc;
    VALUE sym, tok, arr;
{
    D(debug_print(sc, sym, tok));
    rb_ary_store(arr, 0, sym);
    rb_ary_store(arr, 1, tok);
    rb_yield(arr);
}

/*
 * Document-method: mails_scan
 *
 * TODO: Documentation needed
 *
 */
static VALUE
mails_scan(self)
    VALUE self;
{
    struct scanner *sc;
    VALUE arr;

#define PASS(s,v) pass_token(sc,s,v,arr)
    GET_SCANNER(self, sc);
    if (!sc->p) {
        rb_raise(ScanError, "Mails#scan called before reset");
    }
    arr = rb_assoc_new(Qnil, Qnil);

    while (sc->p < sc->pend) {
        D(puts("new loop"));
        D(printf("char='%c'\n", *sc->p));
        if (IS_LWSP(*sc->p)) {
            D(puts("lwsp"));
            skip_lwsp(sc);
            if (sc->p >= sc->pend)
                break;
        }

        if (MIME_MODE_P(sc)) {
            if (IS_TOKENCHAR(*sc->p) ||
                    (ISO2022_MODE_P(sc) && (*sc->p == ESC)) ||
                    IS_JCHAR(*sc->p)) {
                D(puts("token"));
                PASS(tok_token, scan_token(sc));
                continue;
            }
        }
        else {
            if (IS_ATOMCHAR(*sc->p) ||
                    (ISO2022_MODE_P(sc) && (*sc->p == ESC)) ||
                    IS_JCHAR(*sc->p)) {
                VALUE tmp;
                D(puts("atom"));
                tmp = scan_atom(sc);
                PASS(atomsym(sc, tmp), tmp);
                continue;
            }
        }

        if (*sc->p == '"') {
            D(puts("quoted"));
            PASS(tok_quoted, scan_quoted_word(sc));
            D(puts("quoted"));
        }
        else if (*sc->p == '(') {
            VALUE c;
            D(puts("comment"));
            c = scan_comment(sc);
            if (! NIL_P(sc->comments))
                rb_ary_push(sc->comments, c);
        }
        else if (*sc->p == '[') {
            D(puts("domlit"));
            PASS(tok_domlit, scan_domain_literal(sc));
        }
        else {
            VALUE ch;
            D(puts("char"));
            ch = rb_str_new(sc->p, 1);
            sc->p++;
            PASS(ch, ch);
        }
    }

    PASS(Qfalse, rb_str_new("$", 1));
    return Qnil;
}


/*
------------------------------------------------------------------
                         ruby interface
------------------------------------------------------------------
*/

#ifdef rb_intern_const
#define cstr2symbol(str) ID2SYM(rb_intern_const(str))
#else
static VALUE
cstr2symbol(str)
    const char *str;
{
    ID tmp;

    tmp = rb_intern(str);
#ifdef ID2SYM
    return ID2SYM(tmp);
#else
    return INT2FIX(tmp);
#endif
}
#endif

void
Init_tmailscanner()
{
    VALUE TMail;
    VALUE tmp;

    if (rb_const_defined(rb_cObject, rb_intern("TMail"))) {
        TMail = rb_const_get(rb_cObject, rb_intern("TMail"));
    }
    else {
        TMail = rb_define_module("TMail");
    }
    TMailScanner = rb_define_class_under(TMail, "TMailScanner", rb_cObject);

    tmp = rb_str_new2(TMAIL_VERSION);
    rb_obj_freeze(tmp);
    rb_define_const(TMailScanner, "Version", tmp);

#ifdef HAVE_RB_DEFINE_ALLOC_FUNC
    rb_define_alloc_func(TMailScanner, mails_s_alloc);
    rb_define_method(TMailScanner, "initialize", mails_init, 3);
#else
    rb_define_singleton_method(TMailScanner, "new", mails_s_new, 3);
#endif
    rb_define_method(TMailScanner, "scan", mails_scan, 0);
    rb_define_method(TMailScanner, "debug", mails_debug_get, 0);
    rb_define_method(TMailScanner, "debug=", mails_debug_set, 1);

    if (rb_const_defined(TMail, rb_intern("SyntaxError"))) {
        ScanError = rb_const_get(rb_cObject, rb_intern("SyntaxError"));
    }
    else {
        ScanError = rb_define_class_under(TMail, "SyntaxError", rb_eStandardError);
    }

    tok_atom   = cstr2symbol("ATOM");
    tok_digit  = cstr2symbol("DIGIT");
    tok_token  = cstr2symbol("TOKEN");
    tok_quoted = cstr2symbol("QUOTED");
    tok_domlit = cstr2symbol("DOMLIT");

    tok_from   = cstr2symbol("FROM");
    tok_by     = cstr2symbol("BY");
    tok_via    = cstr2symbol("VIA");
    tok_with   = cstr2symbol("WITH");
    tok_id     = cstr2symbol("ID");
    tok_for    = cstr2symbol("FOR");
}

