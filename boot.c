#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define new(TYPE,...)\
    (TYPE*) memcpy(malloc(sizeof(TYPE)), &(TYPE){__VA_ARGS__}, sizeof(TYPE))

typedef struct { int len; char chars[]; } String;
typedef struct { String *name; int line, col; } Pos;

typedef struct Expr Expr;
typedef struct List List;
typedef struct Op Op;
typedef struct Type Type;
typedef struct Swap Swap;
typedef struct Con Con;

struct Expr {
    enum { EINT, ECHAR, ESTRING, ECON, EVAR, ETUPLE, ELIST, EFN,
        EAPP, EIF, ECASE, ELET, EREC, ESEQ, EDEREF } form;
    Pos pos;
    union {
        int n;
        String *str, *id;
        List *tuple, *list;
        struct { List *rules; } fn;
        struct { Expr *f; List *args; } app;
        struct { Expr *a, *b, *c; } _if;
        struct { Expr *subject; List *rules; } _case;
        struct { Expr *lhs, *rhs, *in; } let;
        struct { List *rules; Expr *in; } rec;
        struct { Expr *lhs, *rhs; } seq;
        struct { String *subject, *tag; List *vars; Expr *body, *fail; } test;
        Expr *deref;
    };
};
typedef struct Rule { Expr *lhs, *guard, *rhs; } Rule;
typedef struct FnRule {Pos pos; List *params; Expr *guard; Expr *body; } FnRule;
#define Expr(FORM, POS, ...) new(Expr, FORM, .pos=POS, __VA_ARGS__)
#define Rule(LHS, GUARD, RHS) new(Rule, LHS, GUARD, RHS)
#define FnRule(POS, PARAMS, GUARD, BODY) new(FnRule, POS, PARAMS, GUARD, BODY)

struct Op { String *id; int prec; bool left; Expr *expr; };
#define Op(ID, PREC, LEFT, EXPR) new(Op, ID, PREC, LEFT, EXPR)
struct List {
    union {
        void    *p;
        Expr    *expr;
        Rule    *rule;
        FnRule  *fn_rule;
        Op      *op;
        Type    *type;
        Con     *con;
        Swap    *swap;
    };
    List *next;
};
#define List(ITEM, NEXT) new(List, ITEM, NEXT)

typedef enum { TEOF, TINT, TCHAR, TSTRING, TLPAREN, TRPAREN, TLBRACE, TRBRACE,
    TCOMMA, TINFIXING, TSEMI, TCON, TID, TEQUAL, TARROW, TFN, TOR, TIF, TTHEN,
    TELSE, TCASE, TBAR, TLET, TREC, TAND, TIN, TINFIXL, TINFIXR, TDATATYPE,
    TDEREF,
} Token;

char    *tokname[] = {"eof", "int", "char", "string", "(", ")", "[", "]", ",",
            "`", ";", "uppercase id", "id", "=", "->", "fn", "or", "if", "then",
            "else", "case", "|", "let", "rec", "and", "in", "infixl", "infixr",
            "datatype", "!", 0};

char    source[65536];
char    tokbuf[sizeof source];
char    *src;
char    *line_start;
Token   token;
bool    peeked;
int     tokint;
String  *tokstr;
Pos     srcpos;
String  *interns[65536];
int     ninterns;
List    *infixes;

String *new_string(char *chars, int len) {
    if (len < 0) len = strlen(chars);
    String *out = malloc(sizeof *out + len + 1);
    out->len = len;
    if (chars) memcpy(out->chars, chars, len);
    out->chars[len] = 0;
    return out;
}

#define EACH(LIST) for (List *i = (LIST); i; i = i->next)
int count(List *xs) { return xs? 1 + count(xs->next): 0; }
bool push(List **next, void *item) {
    if (!item) return true;
    return *next = List(item, *next);
}
List *pop(List **list) { List *x = *list; *list = x->next; return x; }
List *reverse(List *xs) {
    List *out = 0;
    EACH(xs) push(&out, i->p);
    return out;
}
bool append(List **next, void *item) {
    if (!item) return false;
    while (*next) next = &(*next)->next;
    return *next = List(item, 0);
}

String *intern(char *chars, int len) {
    if (len < 0) len = strlen(chars);
    for (String **i = interns + ninterns; i-- > interns; )
        if ((*i)->len == len && !memcmp((*i)->chars, chars, len))
            return *i;
    return interns[ninterns++] = new_string(chars, len);
}

void *fatal(Pos pos, char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    printf("boot: error %s:%d:%d: ", pos.name->chars, pos.line, pos.col);
    vprintf(msg, ap);
    puts("");
    exit(1);
}
void set_src(char *name, char *text) {
    src = line_start = source;
    peeked = false;
    srcpos = (Pos) {intern(name, -1), 1, 1};
    memmove(source, text, strlen(text) + 1);
}
bool open_src(char *path) {
    FILE *file = fopen(path, "rb");
    if (!file) return set_src(path, ""), false;
    fread(source, 1, sizeof source, file);
    fclose(file);
    return set_src(path, source), true;
}

int character(void) {
    if (!*src) fatal(srcpos, "unclosed character");
    if (*src == '\n') line_start = src + 1, srcpos.line++, srcpos.col = 1;
    if (*src != '\\') return *src++;
    char *esc = "a\a" "b\b" "e\033" "f\f" "n\n" "r\r" "t\t" "v\v" "0\0";
    for (char *i = (++src, esc); *i; i += 2)
        if (i[0] == *src) return src++, i[1];
    return *src++;
}

Token next(void) {
    if (peeked)
        return peeked = false, token;
    while (true)
        if (*src == '\n') line_start = ++src, srcpos.line++;
        else if (isspace(*src)) src++;
        else if (*src == '#') while (*src && *src != '\n') src++;
        else break;

    srcpos.col = src - line_start + 1;
    if (!*src) return token = TEOF;
    if (isdigit(src[*src == '-']))
        return tokint = strtol(src, &src, 10), token = TINT;
    for (Token t = TLPAREN; t <= TSEMI; t++)
        if (*src == tokname[t][0]) return src++, token = t;
    if (*src == '\'') {
        tokint = (src++, character());
        if (*src++ != '\'') fatal(srcpos, "unclosed quote");
        return token = TCHAR;
    }
    if (*src == '"')
        for (char *s = (++src, tokbuf); ; )
            if (!*src) fatal(srcpos, "unclosed string");
            else if (*src != '"') *s++ = character();
            else {
                src++;
                return tokstr = intern(tokbuf, s - tokbuf), token = TSTRING;
            }
    char *base = src, *symbol = "!$%&*+-/:<=>@^|~";
    while (*src && (isalnum(*src) || strchr("_'?!", *src))) src++;
    if (base == src) while (*src && strchr(symbol, *src)) src++;
    if (base == src) fatal(srcpos, "bad token: %c", *src);
    tokstr = intern(base, src - base);
    for (Token t = TID + 1; tokname[t]; t++)
        if (!strcmp(tokname[t], tokstr->chars)) return token = t;
    return token = isupper(*base) || *base == ':'? TCON: TID;
}

bool peek(Token t) { return next(), peeked = true, t == token; }
bool want(Token t) { return !(peeked = next() != t); }
void need(Token t) { if (!want(t)) fatal(srcpos, "need %s", tokname[t]); }

Expr *expr(void);
Expr *atexpr(bool required);

Op *infix(void) {
    if (peek(TID) || peek(TCON))
        EACH(infixes) if (i->op->id == tokstr) return i->op;
    return 0;
}
List *csv(Token delim) {
    if (want(delim)) return 0;
    void    *hd = expr();
    return List(hd, want(TCOMMA)? csv(delim): (need(delim), NULL));
}
List *fn_rules(Token delim) {
    Pos     pos = srcpos;
    List    *params = 0;
    while (append(&params, atexpr(false)));
    Expr    *guard = want(TIF)? expr(): 0;
    Expr    *body = (need(delim), expr());
    FnRule  *hd = FnRule(pos, params, guard, body);
    return List(hd, want(TOR)? fn_rules(delim): 0);
}
Expr *function(Pos pos, Token delim) {
    if (want(delim)) return expr();
    return Expr(EFN, pos, .fn={fn_rules(delim)});
}
Expr *atexpr(bool required) {
    Pos     pos = srcpos;
    List    *xs;
    if (!required && (peek(TID) || peek(TCON)) && infix()) return 0;
    switch (next()) {
    case TINT:  return Expr(EINT, pos, .n=tokint);
    case TCHAR: return Expr(ECHAR, pos, .n=tokint);
    case TSTRING: return Expr(ESTRING, pos, .str=tokstr);
    case TCON:  return Expr(ECON, pos, .id=tokstr);
    case TID:   return Expr(EVAR, pos, .id=tokstr);
    case TDEREF:return Expr(EDEREF, pos, .deref=atexpr(true));
    case TLPAREN:   xs = csv(TRPAREN);
                    if (count(xs) == 1) return xs->expr;
                    return Expr(ETUPLE, pos, .tuple=xs);
    case TLBRACE:   return Expr(ELIST, pos, .list=csv(TRBRACE));
    case TFN:       return function(pos, TARROW);
    default:        peeked = true;
                    if (required) fatal(srcpos, "need expression");
                    return 0;
    }
}
Expr *appexpr(void) {
    Expr    *f = atexpr(true), *x;
    List    *args = 0;
    while (append(&args, atexpr(false)));
    return args? Expr(EAPP, f->pos, .app={f, args}): f;
}
void reduce_ops(List **operands, List **ops, int prec, bool left) {
    int p;
    while (*ops && (p = (*ops)->op->prec) &&
           (prec < p || (prec == p && left)))
    {
        Expr    *f = pop(ops)->op->expr;
        Expr    *y = pop(operands)->expr;
        Expr    *x = pop(operands)->expr;
        push(operands, Expr(EAPP, x->pos, .app={f, List(x, List(y, 0))}));
    }
}
Expr *infexpr(void) {
    List    *operands = List(appexpr(), 0);
    List    *operators = 0;
    Op      *op;
    while ((op = infix())) {
        Expr    *f = (want(TINFIXING), atexpr(true));
        reduce_ops(&operands, &operators, op->prec, op->left);
        push(&operators, Op(f->id, op->prec, op->left, f));
        push(&operands, appexpr());
    }
    return reduce_ops(&operands, &operators, 0, true), operands->expr;
}
Expr *let_defs(void) {
    Pos pos = srcpos;
    if (want(TREC)) {
        List    *rules = 0;
        want(TAND);
        do {
            Expr *id = (need(TID), Expr(EVAR, srcpos, .id=tokstr));
            Expr *fn = function(srcpos, TEQUAL);
            append(&rules, Rule(id, 0, fn));
        } while (want(TAND));
        Expr    *body = want(TLET)? let_defs(): (need(TIN), expr());
        return Expr(EREC, pos, .rec={rules, body});
    } else {
        Expr    *lhs = atexpr(true);
        Expr    *rhs = function(srcpos, TEQUAL);
        Expr    *body = want(TLET)? let_defs(): (need(TIN), expr());
        return Expr(ELET, pos, .let={lhs, rhs, body});
    }
}
Expr *expr(void) {
    Pos pos = srcpos;
    Expr    *e;
    if (want(TIF)) {
        Expr    *a = expr();
        Expr    *b = (need(TTHEN), expr());
        Expr    *c = (need(TELSE), expr());
        e = Expr(EIF, pos, ._if={a, b, c});
    } else if (want(TCASE)) {
        Expr    *subject = expr();
        List    *rules = 0;
        while (want(TBAR)) {
            Expr    *lhs = expr();
            Expr    *guard = want(TIF)? expr(): 0;
            Expr    *rhs = (need(TARROW), expr());
            append(&rules, Rule(lhs, guard, rhs));
        }
        e = Expr(ECASE, pos, ._case={subject, rules});
    } else if (want(TLET)) e = let_defs();
    else e = infexpr();
    return want(TSEMI)? Expr(ESEQ, e->pos, .seq={e, expr()}): e;
}

struct Type {
    enum { TYPE, TYPEVAR, TUPLE_TYPE, FN_TYPE } form;
    String  *id;
    Type    *inst;
    List    *args;
};
struct Swap { Type *from, *to; };
struct Con { String *tag; Type *type; };
typedef struct Static { String *id; Type *type; struct Static *next; } Static;
#define Con(TAG, TYPE) new(Con, TAG, TYPE)
#define Static(ID, TYPE, NEXT) new(Static, ID, TYPE, NEXT)

List        *all_types;
List        *all_cons;
List        *nongeneric;
Type        *bool_type, *int_type, *char_type, *string_type, *unit_type, *list_type, *ref_type;
String      *wildcard, *ref_tag, *tuple_tag, *list_tag, *true_tag;

Type *basic(String *id, List *args) { return new(Type, TYPE, id, 0, args); }
Type *typevar(String *id) { return new(Type, TYPEVAR, id, 0, 0); }
Type *tuple_type(List *args) {
    if (count(args) == 1) return args->type;
    return new(Type, TUPLE_TYPE, 0, 0, args);
}
Type *fn_type(List *args) {
    if (count(args) == 1) return args->type;
    return new(Type, FN_TYPE, 0, 0, args);
}
Type *prune(Type *t) { return t->inst? (t->inst = prune(t->inst)): t; }
bool occurs_in(Type *var, Type *type) {
    type = prune(type);
    if (type->form == TYPEVAR) return type == var;
    EACH(type->args) if (occurs_in(var, i->type)) return true;
    return false;
}
void rename_typevars(Type *t, int *unique) {
    t = prune(t);
    if (t->form == TYPEVAR && !t->id)
        t->id = intern((char[]){'\'', (*unique)++ + 'a'}, 2);
    EACH(t->args) rename_typevars(i->type, unique);
}
char *wt(char *buf, Type *t, bool paren);
char *wts(char *buf, List *list, char *separator, bool paren) {
    if (!list) return buf;
    wt(buf, list->type, paren);
    EACH(list->next) strcat(buf, separator), wt(buf, i->type, paren);
    return buf;
}
char *wt(char *buf, Type *t, bool paren) {
    t = prune(t);
    switch (t->form) {
    case TYPE:          if (!t->args) return strcat(buf, t->id->chars);
                        wts(buf, t->args, " and ", true);
                        return strcat(strcat(buf, " "), t->id->chars);
    case TYPEVAR:       return strcat(buf, t->id->chars);
    case TUPLE_TYPE:    wts(strcat(buf, "("), t->args, ", ", false);
                        return strcat(buf, ")");
    case FN_TYPE:       return !paren? wts(buf, t->args, " -> ", true):
                            strcat(wt(strcat(buf, "("), t, false), ")");
    }
}
bool unifies(Type *t, Type *u) {
    t = prune(t), u = prune(u);
    if (t->form == TYPEVAR)
        if (occurs_in(t, u)) return t == u;
        else return t->inst = u;
    if (u->form == TYPEVAR) return unifies(u, t);
    if (t->id != u->id) return false;
    List *i = t->args, *j = u->args;
    for ( ; i && j; i = i->next, j = j->next)
        if (!unifies(i->type, j->type)) return false;
    return !i == !j;
}
Type *unify(Pos pos, Type *want, Type *got) {
    want = prune(want), got = prune(got);
    if (!unifies(want, got)) {
        char msg[256] = "type mismatch:\na: ";
        int unique = 0;
        rename_typevars(want, &unique);
        rename_typevars(got, &unique);
        wt(msg, want, false);
        wt(strcat(msg, "\nb: "), got, false);
        fatal(pos, msg);
    }
    return want;
}
Type *fresh(Type *type, List **swaps) {
    List *tmp = 0;
    if (!swaps) swaps = &tmp;
    type = prune(type);
    if (type->form == TYPEVAR) {
        EACH(nongeneric) if (occurs_in(type, i->type)) return type;
        EACH(*swaps) if (i->swap->from == type) return i->swap->to;
        Type *to = typevar(0);
        push(swaps, new(Swap, type, to));
        return to;
    }
    List *args = 0;
    EACH(type->args) append(&args, fresh(i->type, swaps));
    return new(Type, type->form, type->id, 0, args);
}
Type *find_type(Pos pos, String *id, bool required) {
    EACH(all_types) if (i->type->id == id) return prune(i->type);
    if (required) fatal(pos, "undefined type: %s", id->chars);
    return 0;
}
Con *find_con(Pos pos, String *tag, bool required) {
    EACH(all_cons) if (i->con->tag == tag) return i->con;
    if (required) fatal(pos, "undefined constructor: %s", tag->chars);
    return 0;
}
Static *find_static(Pos pos, String *id, Static *env) {
    for (Static *i = env; i; i = i->next) if (i->id == id) return i;
    fatal(pos, "undefined: %s", id->chars);
}
Type *type(void);
Type *attype(bool required) {
    if (want(TLPAREN)) {
        List    *vals = 0;
        do {
            if (peek(TRPAREN)) break;
            append(&vals, type());
        } while (want(TCOMMA));
        need(TRPAREN);
        return tuple_type(vals);
    } else if (want(TID) || want(TCON)) {
        Type *t = find_type(srcpos, tokstr, true);
        if (t->args) fatal(srcpos, "type needs args");
        return t;
    }
    return required? fatal(srcpos, "need type"): 0;
}
Type *apptype(void) {
    List    *args = List(attype(true), 0);
    while (want(TAND)) append(&args, attype(true));
    while (want(TID) || want(TCON)) {
        Type    *t = find_type(srcpos, tokstr, true);
        if (t->form != TYPE || count(t->args) != count(args))
            fatal(srcpos, "wrong type args: %s", t->id->chars);
        args = List(basic(t->id, args), 0);
    }
    if (count(args) != 1) fatal(srcpos, "need type constructor");
    return args->type;
}
Type *type(void) {
    List    *signature = List(apptype(), 0);
    while (want(TARROW)) append(&signature, apptype());
    return fn_type(signature);
}
void header(void) {
    while (true)
        if (want(TINFIXL) || want(TINFIXR)) {
            bool left = token == TINFIXL;
            int prec = (need(TINT), tokint);
            while (want(TID) || want(TCON))
                push(&infixes, Op(tokstr, prec, left, 0));
        } else if (want(TDATATYPE)) {
            Pos     pos = srcpos;
            List    *params = 0;
            while (want(TID) || want(TCON)) push(&params, typevar(tokstr));
            String  *id = pop(&params)->type->id;
            Type    *dt = basic(id, reverse(params));
            if (find_type(pos, dt->id, false))
                fatal(pos, "redefined: %s", dt->id->chars);
            push(&all_types, dt);
            List    *old_types = all_types;
            EACH(params) push(&all_types, i->type);
            need(TEQUAL), want(TBAR);
            do {
                String  *tag = (need(TCON), tokstr);
                if (find_con(srcpos, tag, false))
                    fatal(srcpos, "redefined: %s", tag->chars);
                List    *signature = 0;
                while (append(&signature, attype(false)));
                append(&signature, dt);
                push(&all_cons, Con(tag, fn_type(signature)));
            } while (want(TBAR));
            all_types = old_types;
        } else break;
}
bool is_value(Expr *e) {
    switch (e->form) {
    case EINT: case ECHAR: case ESTRING: case ECON: case EVAR:
        return true;
    case ETUPLE:    EACH(e->tuple) if (!is_value(i->expr)) return false;
                    return true;
    case ELIST:     EACH(e->list) if (!is_value(i->expr)) return false;
                    return true;
    case EFN:       return true;
    case EAPP:      if (e->app.f->form != ECON) return false;
                    if (e->app.f->id == ref_tag) return false;
                    EACH(e->app.args) if (!is_value(i->expr)) return false;
                    return true;
    default:        return false;
    }
}
Type *tc_pat(Expr *e, Static **env) {
    List    *xs = 0;
    Type    *t, *u;
    switch (e->form) {
    case EINT:      return int_type;
    case ECHAR:     return char_type;
    case ESTRING:   return string_type;
    case ECON:      return fresh(find_con(e->pos, e->id, true)->type, 0);
    case EVAR:      t = typevar(0);
                    if (e->id != wildcard) *env = Static(e->id, t, *env);
                    push(&nongeneric, t);
                    return t;
    case ETUPLE:    EACH(e->tuple) append(&xs, tc_pat(i->expr, env));
                    return tuple_type(xs);
    case ELIST:     t = typevar(0);
                    EACH(e->list) unify(i->expr->pos, t, tc_pat(i->expr, env));
                    return basic(list_type->id, List(t, 0));
    case EAPP:      if (e->app.f->form != ECON) goto invalid;
                    t = typevar(0);
                    u = tc_pat(e->app.f, env);
                    EACH(e->app.args) append(&xs, tc_pat(i->expr, env));
                    append(&xs, t);
                    unify(e->pos, u, fn_type(xs));
                    return prune(t);
    }
    invalid: fatal(e->pos, "invalid pattern");
}
Type *tc(Expr *e, Static *env) {
    List    *xs = 0;
    Type    *t, *u;
    List    *old_nongeneric = nongeneric;
    switch (e->form) {
    case EINT:      return int_type;
    case ECHAR:     return char_type;
    case ESTRING:   return string_type;
    case ECON:      return fresh(find_con(e->pos, e->id, true)->type, 0);
    case EVAR:      return fresh(find_static(e->pos, e->id, env)->type, 0);
    case ETUPLE:    EACH(e->tuple) append(&xs, tc(i->expr, env));
                    return tuple_type(xs);
    case ELIST:     t = typevar(0);
                    EACH(e->list) unify(i->expr->pos, t, tc(i->expr, env));
                    return basic(list_type->id, List(t, 0));
    case EDEREF:    u = tc(e->deref, env);
                    t = typevar(0);
                    unify(e->pos, basic(ref_type->id, List(t, 0)), u);
                    return t;
    case EFN:       t = typevar(0);
                    EACH(e->fn.rules) {
                        FnRule  *f = i->fn_rule;
                        Static  *local = env;
                        List    *signature = 0;
                        EACH(f->params) append(&signature, tc_pat(i->expr, &local));
                        if (f->guard)
                            unify(f->guard->pos, bool_type, tc(f->guard, local));
                        append(&signature, tc(f->body, local));
                        unify(f->pos, t, fn_type(signature));
                    }
                    nongeneric = old_nongeneric;
                    return prune(t);
    case EAPP:      t = typevar(0);
                    u = tc(e->app.f, env);
                    EACH(e->app.args) append(&xs, tc(i->expr, env));
                    append(&xs, t);
                    unify(e->app.f->pos, u, fn_type(xs));
                    return prune(t);
    case EIF:       unify(e->_if.a->pos, bool_type, tc(e->_if.a, env));
                    t = tc(e->_if.b, env);
                    return unify(e->_if.c->pos, t, tc(e->_if.c, env));
    case ECASE:     t = tc(e->_case.subject, env);
                    u = typevar(0);
                    EACH(e->_case.rules) {
                        Expr    *lhs = i->rule->lhs, *rhs = i->rule->rhs,
                                *guard = i->rule->guard;
                        Static  *local = env;
                        unify(lhs->pos, t, tc_pat(lhs, &local));
                        if (guard)
                            unify(guard->pos, bool_type, tc(guard, local));
                        unify(rhs->pos, u, tc(rhs, local));
                    }
                    nongeneric = old_nongeneric;
                    return prune(u);
    case ELET:      t = tc_pat(e->let.lhs, &env),
                    unify(e->let.lhs->pos, tc(e->let.rhs, env), t);
                    if (is_value(e->let.rhs)) // Look up "Value restriction".
                        nongeneric = old_nongeneric;
                    t = tc(e->let.in, env);
                    nongeneric = old_nongeneric;
                    return t;
    case EREC:      EACH(e->rec.rules) {
                        Expr *lhs = i->rule->lhs, *rhs = i->rule->rhs;
                        if (lhs->form != EVAR) fatal(lhs->pos, "l.h.s. must be ID");
                        if (rhs->form != EFN) fatal(rhs->pos, "r.h.s. must be function");
                        env = Static(lhs->id, t = typevar(0), env);
                        push(&nongeneric, t);
                    }
                    EACH(e->rec.rules) {
                        Expr *lhs = i->rule->lhs, *rhs = i->rule->rhs;
                        t = find_static(lhs->pos, lhs->id, env)->type;
                        unify(lhs->pos, t, tc(rhs, env));
                    }
                    nongeneric = old_nongeneric;
                    return tc(e->rec.in, env);
    case ESEQ:      return tc(e->seq.lhs, env), tc(e->seq.rhs, env);
    }
    fatal(e->pos, "UNHANDLED ASSIGN");
}

typedef struct Value Value;
typedef struct LC LC;
typedef struct Dynamic Dynamic;
struct Value {
    enum { INT, CHAR, STRING, DATA, FN, PRIM } form;
    union {
        int         n;
        String      *str;
        struct Data *data;
        struct Fn   *fn;
        int         prim;
    };
};
typedef struct Data { String *tag; int n; Value xs[]; } Data;
typedef struct Fn { LC *body; Dynamic *env; } Fn;
struct Dynamic { Value value; Dynamic *next; };
#define Dynamic(VALUE, NEXT) new(Dynamic, VALUE, NEXT)

typedef enum {
    PADD, PSUB, PMUL, PDIV, PREM, PEQUAL, PNOTEQUAL,
} Prim;
struct prim_spec { char *id, *sig; } prim_spec[] = {
    {"+", "int->int->int"},
    {"-", "int->int->int"},
    {"*", "int->int->int"},
    {"/", "int->int->int"},
    {"rem", "int->int->int"},
    {"==", "a -> a -> bool"},
    {"<>", "a -> a -> bool"},
    {0, 0}
};

bool equal(Value a, Value b) {
    switch (a.form) {
    case INT:   return a.n == b.n;
    case CHAR:  return a.n == b.n;
    case STRING: if (a.str == b.str) return true;
                if (a.str->len != b.str->len) return false;
                return !memcmp(a.str->chars, b.str->chars, a.str->len);
    case DATA:  if (a.data->tag != b.data->tag) return false;
                for (int i = 0; i < a.data->n; i++)
                    if (!equal(a.data->xs[i], b.data->xs[i])) return false;
                return true;
    case FN:    return a.fn == b.fn;
    case PRIM:  return a.prim == b.prim;
    }
}

struct LC {
    enum { LLIT, LVAR, LTUPLE, LFN, LAPP, LIF, LLET, LREC, LSEQ  } form;
    Pos pos;
    union {
        Value   lit;
        int     index;
        struct { int n; LC **xs; } tuple;
        struct { LC *body; } fn;
        struct { LC *f; int n; LC **args; } app;
        struct { LC *a, *b, *c; } _if;
        struct { LC *value, *in; } let;
        struct { int n; LC **bodies, *in; } rec;
        struct { LC *lhs, *rhs; } seq;
    };
};
#define LC(FORM, POS, ...) new(LC, FORM, .pos=POS, __VA_ARGS__)

Value   unit, nil, list_con, _true, _false;

Value the_int(int n) { return (Value) {INT, .n=n}; }
Value the_char(int n) { return (Value) {CHAR, .n=n}; }
Value the_str(String *str) { return (Value) {STRING, .str=str}; }
Value the_data(String *tag, int n, Value *xs) {
    Data *data = malloc(sizeof *data + n * sizeof *xs);
    data->tag = tag;
    data->n = n;
    if (xs) memcpy(data->xs, xs, n * sizeof *xs);
    return (Value) {DATA, .data=data};
}
Value the_fn(LC *body, Dynamic *env) {
    return (Value) {FN, .fn=new(Fn, body, env)};
}
Value the_prim(int prim) { return (Value) {PRIM, .prim=prim}; }
void pv(Value x) {
    switch (x.form) {
    case INT:   printf("%d", x.n); break;
    case CHAR:  putchar(x.n); break;
    case STRING:fwrite(x.str->chars, x.str->len, 1, stdout); break;
    case DATA:  if (x.data->tag == tuple_tag) {
                    putchar('(');
                    for (int i = 0; i < x.data->n; i++)
                        fputs(i? ", ": "", stdout), pv(x.data->xs[i]);
                    putchar(')');
                } else if (x.data->tag == list_tag && x.data->n) {
                    putchar('[');
                    for (int i=0; x.data->tag == list_tag; x=x.data->xs[1], i++)
                        fputs(i? ", ": "", stdout), pv(x.data->xs[0]);
                    putchar(']');
                } else if (x.data->n) {
                    putchar('(');
                    fputs(x.data->tag->chars, stdout);
                    for (int i = 0; i < x.data->n; i++)
                        putchar(' '), pv(x.data->xs[i]);
                    putchar(')');
                } else fputs(x.data->tag->chars, stdout);
                break;
    case FN:    fputs("#fn", stdout); break;
    case PRIM:  fputs(prim_spec[x.prim].id, stdout); break;
    }
}
LC *lc(Expr *e, Static *env);
LC *lit(Pos pos, Value x) { return LC(LLIT, pos, .lit=x); }
LC *lc_fn(Expr *e, Static *env) {
    if (count(e->fn.rules) == 1) {
        List    *params = e->fn.rules->fn_rule->params;
        bool    simple = e->fn.rules->fn_rule->guard == 0;
        EACH(params) simple &= i->expr->form == EVAR;
        if (simple) {
            EACH(params) env = Static(i->expr->id, 0, env);
            return LC(LFN, e->pos, .fn={lc(e->fn.rules->fn_rule->body, env)});
        }
    }
    fatal(e->pos, "UNHANDLED LC_FN");
}
int debruijn(String *id, Static *env) {
    return env->id == id? 0: 1 + debruijn(id, env->next);
}
LC *lc(Expr *e, Static *env) {
    LC      **xs, *a, *b, *c, *f;
    Expr    *fail = 0;
    int     n = 0;
    switch (e->form) {
    case EINT:      return lit(e->pos, the_int(e->n));
    case ECHAR:     return lit(e->pos, the_char(e->n));
    case ESTRING:   return lit(e->pos, the_str(e->str));
    case ECON:      return lit(e->pos, the_data(e->id, 0, 0));
    case EVAR:      return LC(LVAR, e->pos, .index=debruijn(e->id, env));
    case ETUPLE:    if (!e->tuple) return lit(e->pos, unit);
                    xs = malloc(count(e->tuple) * sizeof *xs);
                    EACH(e->tuple) xs[n++] = lc(i->expr, env);
                    return LC(LTUPLE, e->pos, .tuple={n, xs});
    case ELIST:     c = lit(e->pos, nil);
                    EACH(reverse(e->list))
                        f = lit(i->expr->pos, list_con),
                        xs = malloc(2 * sizeof *xs),
                        xs[0] = lc(i->expr, env),
                        xs[1] = c,
                        c = LC(LAPP, xs[0]->pos, .app={f, 2, xs});
                    return c;
    case EFN:       return lc_fn(e, env);
    case EAPP:      xs = malloc(count(e->app.args) * sizeof *xs);
                    f = lc(e->app.f, env);
                    EACH(e->app.args) xs[n++] = lc(i->expr, env);
                    return LC(LAPP, e->pos, .app={f, n, xs});
    case EIF:       a = lc(e->_if.a, env);
                    b = lc(e->_if.b, env);
                    return LC(LIF, e->pos, ._if={a, b, lc(e->_if.c, env)});
    // case ECASE:
    case ELET:      if (e->let.lhs->form == EVAR) {
                        LC      *value = lc(e->let.rhs, env);
                        Static  *local = Static(e->let.lhs->id, 0, env);
                        LC      *in = lc(e->let.in, local);
                        return LC(LLET, e->pos, .let={value, in});
                    }
                    goto untranslated;
    case EREC:      xs = malloc(count(e->rec.rules) * sizeof *xs);
                    EACH(e->rec.rules) env = Static(i->rule->lhs->id, 0, env);
                    EACH(e->rec.rules) xs[n++] = lc(i->rule->rhs, env)->fn.body;
                    c = lc(e->rec.in, env);
                    return LC(LREC, e->pos, .rec={n, xs, c});
    case ESEQ:      return LC(LSEQ, e->pos, .seq={lc(e->seq.lhs, env),
                                                  lc(e->seq.rhs, env)});
    // case EDEREF:
    }
    untranslated: fatal(e->pos, "UNTRANSLATED EXPRESSION");
}
Value eval(LC *c, Dynamic *env) {
    Value   *xs, f, x;
    Dynamic *old;
    top:
    switch (c->form) {
    case LLIT:  return c->lit;
    case LVAR:      for (int i = 0; i < c->index; i++) env = env->next;
                    return env->value;
    case LTUPLE:    xs = malloc(c->tuple.n * sizeof *xs);
                    for (int i = 0; i < c->tuple.n; i++)
                        xs[i] = eval(c->tuple.xs[i], env);
                    return the_data(tuple_tag, c->tuple.n, xs);
    case LFN:       return the_fn(c->fn.body, env);
    case LAPP:      f = eval(c->app.f, env);
                    if (f.form == PRIM) {
                        Value   x[4];
                        for (int i = 0; i < c->app.n; i++)
                            x[i] = eval(c->app.args[i], env);
                        switch (f.prim) {
                        case PADD:      return the_int(x[0].n + x[1].n);
                        case PSUB:      return the_int(x[0].n - x[1].n);
                        case PMUL:      return the_int(x[0].n * x[1].n);
                        case PDIV:      return the_int(x[0].n / x[1].n);
                        case PREM:      return the_int(x[0].n % x[1].n);
                        case PEQUAL:    return equal(x[0], x[1])? _true: _false;
                        case PNOTEQUAL: return equal(x[0], x[1])? _false: _true;
                        }
                    } else if (f.form == DATA) {
                        Value out = the_data(f.data->tag, c->app.n, 0);
                        for (int i = 0; i < c->app.n; i++)
                            out.data->xs[i] = eval(c->app.args[i], env);
                        return out;
                    }
                    old = env;
                    env = f.fn->env;
                    for (int i = 0; i < c->app.n; i++)
                        env = Dynamic(eval(c->app.args[i], old), env);
                    c = f.fn->body;
                    goto top;
    case LIF:       c = eval(c->_if.a, env).data->tag == true_tag
                            ? c->_if.b
                            : c->_if.c;
                    goto top;
    case LLET:      env = Dynamic(eval(c->let.value, env), env);
                    c = c->let.in;
                    goto top;
    case LREC:      old = env;
                    for (int i = 0; i < c->rec.n; i++)
                        env = Dynamic(the_fn(c->rec.bodies[i], env), env);
                    for (Dynamic *i = env; i != old; i = i->next)
                        i->value.fn->env = env;
                    c = c->rec.in;
                    goto top;
    case LSEQ:  eval(c->seq.lhs, env); c = c->seq.rhs; goto top;
    }
    unevaluated: fatal(c->pos, "UNEVALUATED EXPRESSION");
}
Type *parse_type(char *spec) {
    push(&all_types, typevar(intern("a", -1)));
    Type *out = (set_src("", spec), type());
    pop(&all_types);
    return out;
}
void inititalize(Static **senv, Dynamic **denv) {
    wildcard = intern("_", 1);
    Type *a = typevar(intern("a", -1));
    push(&all_types, bool_type = basic(intern("bool", -1), 0));
    push(&all_types, int_type = basic(intern("int", -1), 0));
    push(&all_types, char_type = basic(intern("char", -1), 0));
    push(&all_types, string_type = basic(intern("string", -1), 0));
    push(&all_types, list_type = basic(intern("list", -1), List(a, 0)));
    push(&all_types, ref_type = basic(intern("ref", -1), List(a, 0)));
    unit_type = tuple_type(0);
    true_tag = intern("TRUE", -1);
    ref_tag = intern("REF", -1);
    tuple_tag = intern("!TUPLE", -1);
    list_tag = intern(":", -1);
    push(&all_cons, Con(true_tag, bool_type));
    push(&all_cons, Con(intern("FALSE", -1), bool_type));
    push(&all_cons, Con(list_tag, parse_type("a -> a list -> a list")));
    push(&all_cons, Con(intern("[]", -1), list_type));
    push(&all_cons, Con(ref_tag, parse_type("a -> a ref")));
    unit = the_data(tuple_tag, 0, 0);
    nil = the_data(intern("[]", -1), 0, 0);
    list_con = the_data(list_tag, 0, 0);
    _true = the_data(true_tag, 0, 0);
    _false = the_data(intern("FALSE", -1), 0, 0);
    for (struct prim_spec *i = prim_spec; i->id; i++)
        *senv = Static(intern(i->id, -1), parse_type(i->sig), *senv),
        *denv = Dynamic(the_prim(i - prim_spec), *denv);
}
int main(int argc, char **argv) {
    Static  *senv = 0;
    Dynamic *denv = 0;
    inititalize(&senv, &denv);
    if (!open_src(argv[1]))
        fatal(srcpos, "cannot open source");
    header();
    Expr *e = expr();
    Type *t = tc(e, senv);

    char buf[1024];
    int unique = 0;
    rename_typevars(t, &unique);
    printf(":: %s\n", wt(buf, t, true));

    LC *c = lc(e, senv);
    Value result = eval(c, denv);
    pv(result), puts("");
    puts("done.");
}
