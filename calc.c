// Demirkan Aktosun 241ADB124
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: Chatgbt= https://chatgpt.com/share/6add03bc-a13c-83eb-afb9-cc7fa67387fd

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <errno.h>
#include <dirent.h>
#include <sys/stat.h>

// AI (ChatGPT), prompt: "Can you make and explain this lab?" The tagged Value stores
// either an integer or a double so integer division keeps C-style truncation.
typedef struct {
    int is_float;
    long long integer;
    double real;
} Value;

typedef struct {
    const char *text;
    size_t pos;
    size_t end;
    size_t file_length;
    size_t error_pos;
    int failed;
} Parser;

static const char *first_name = "firstname";
static const char *last_name = "lastname";
static const char *student_id = "studentid";

static void fail_at(Parser *p, size_t pos)
{
    if (!p->failed) {
        p->failed = 1;
        p->error_pos = pos;
    }
}

static void skip_spaces(Parser *p)
{
    while (p->pos < p->end &&
           (p->text[p->pos] == ' ' || p->text[p->pos] == '\t' ||
            p->text[p->pos] == '\r')) {
        p->pos++;
    }
}

static size_t token_start(Parser *p)
{
    skip_spaces(p);
    if (p->pos < p->end) {
        return p->pos + 1;
    }
    /* The newline itself is the error token; at EOF use length + 1. */
    return p->end < p->file_length ? p->end + 1 : p->file_length + 1;
}

static Value parse_expr(Parser *p);

static Value make_int(long long n)
{
    Value v;
    v.is_float = 0;
    v.integer = n;
    v.real = (double)n;
    return v;
}

static Value make_float(double n)
{
    Value v;
    v.is_float = 1;
    v.integer = 0;
    v.real = n;
    return v;
}

static double as_double(Value v)
{
    return v.is_float ? v.real : (double)v.integer;
}

// AI (ChatGPT), prompt: "Explain the parser grammar and exact error positions."
// This recursive-descent parser keeps each precedence level in its own function.
static Value parse_primary(Parser *p)
{
    Value bad = make_int(0);
    size_t start;
    char c;

    skip_spaces(p);
    start = p->pos;
    if (p->pos >= p->end) {
        fail_at(p, token_start(p));
        return bad;
    }

    c = p->text[p->pos];
    if (c == '(') {
        Value inside;
        p->pos++;
        inside = parse_expr(p);
        if (p->failed) {
            return bad;
        }
        skip_spaces(p);
        if (p->pos >= p->end || p->text[p->pos] != ')') {
            fail_at(p, token_start(p));
            return bad;
        }
        p->pos++;
        return inside;
    }

    if (isdigit((unsigned char)c)) {
        size_t digits_start = p->pos;
        size_t q;
        int has_dot = 0;
        char *endptr;

        while (p->pos < p->end &&
               isdigit((unsigned char)p->text[p->pos])) {
            p->pos++;
        }
        if (p->pos < p->end && p->text[p->pos] == '.') {
            has_dot = 1;
            p->pos++;
            if (p->pos >= p->end ||
                !isdigit((unsigned char)p->text[p->pos])) {
                /* A decimal point without following digits is invalid at the dot. */
                fail_at(p, p->pos);
                return bad;
            }
            while (p->pos < p->end &&
                   isdigit((unsigned char)p->text[p->pos])) {
                p->pos++;
            }
        }

        q = p->pos;
        {
            size_t length = q - digits_start;
            char *number = (char *)malloc(length + 1);
            if (number == NULL) {
                fail_at(p, digits_start + 1);
                return bad;
            }
            memcpy(number, p->text + digits_start, length);
            number[length] = '\0';
            errno = 0;
            if (has_dot) {
                double d = strtod(number, &endptr);
                free(number);
                if (errno == ERANGE || !isfinite(d)) {
                    fail_at(p, digits_start + 1);
                    return bad;
                }
                return make_float(d);
            } else {
                long long n = strtoll(number, &endptr, 10);
                free(number);
                if (errno == ERANGE) {
                    /* The specification says test integers do not overflow. */
                    fail_at(p, digits_start + 1);
                    return bad;
                }
                return make_int(n);
            }
        }
    }

    fail_at(p, start + 1);
    return bad;
}

/* power() parses its right side by calling itself, making ** right-associative. */
static Value parse_power(Parser *p)
{
    Value left = parse_primary(p);
    size_t op_pos;
    size_t rhs_pos;
    Value right;

    if (p->failed) {
        return make_int(0);
    }
    skip_spaces(p);
    if (p->pos + 1 >= p->end ||
        p->text[p->pos] != '*' || p->text[p->pos + 1] != '*') {
        return left;
    }

    op_pos = p->pos + 1;
    p->pos += 2;
    rhs_pos = token_start(p);
    right = parse_power(p);
    if (p->failed) {
        return make_int(0);
    }

    if (!left.is_float && !right.is_float) {
        long long exponent = right.integer;
        long long base = left.integer;
        long long result = 1;
        long long factor = base;

        if (exponent < 0) {
            fail_at(p, rhs_pos);
            return make_int(0);
        }
        while (exponent > 0) {
            if ((exponent & 1LL) != 0) {
                result *= factor;
            }
            exponent >>= 1;
            if (exponent > 0) {
                factor *= factor;
            }
        }
        return make_int(result);
    } else {
        double result = pow(as_double(left), as_double(right));
        if (!isfinite(result)) {
            fail_at(p, op_pos);
            return make_int(0);
        }
        return make_float(result);
    }
}

/* term handles * and / before expr handles + and -, enforcing precedence. */
static Value parse_term(Parser *p)
{
    Value left = parse_power(p);

    while (!p->failed) {
        char op;
        size_t op_pos;
        size_t rhs_pos;
        Value right;

        skip_spaces(p);
        if (p->pos >= p->end ||
            (p->text[p->pos] != '*' && p->text[p->pos] != '/')) {
            break;
        }
        if (p->text[p->pos] == '*' && p->pos + 1 < p->end &&
            p->text[p->pos + 1] == '*') {
            break;
        }

        op = p->text[p->pos];
        op_pos = p->pos + 1;
        p->pos++;
        rhs_pos = token_start(p);
        right = parse_power(p);
        if (p->failed) {
            break;
        }

        if (op == '/' &&
            ((right.is_float && right.real == 0.0) ||
             (!right.is_float && right.integer == 0))) {
            fail_at(p, rhs_pos);
            break;
        }

        if (left.is_float || right.is_float) {
            double a = as_double(left);
            double b = as_double(right);
            double result = (op == '*') ? a * b : a / b;
            if (!isfinite(result)) {
                fail_at(p, op_pos);
                break;
            }
            left = make_float(result);
        } else if (op == '*') {
            left = make_int(left.integer * right.integer);
        } else {
            left = make_int(left.integer / right.integer);
        }
    }
    return left;
}

static Value parse_expr(Parser *p)
{
    Value left = parse_term(p);

    while (!p->failed) {
        char op;
        size_t op_pos;
        Value right;

        skip_spaces(p);
        if (p->pos >= p->end ||
            (p->text[p->pos] != '+' && p->text[p->pos] != '-')) {
            break;
        }

        op = p->text[p->pos];
        op_pos = p->pos + 1;
        p->pos++;
        right = parse_term(p);
        if (p->failed) {
            break;
        }

        if (left.is_float || right.is_float) {
            double a = as_double(left);
            double b = as_double(right);
            double result = (op == '+') ? a + b : a - b;
            if (!isfinite(result)) {
                fail_at(p, op_pos);
                break;
            }
            left = make_float(result);
        } else if (op == '+') {
            left = make_int(left.integer + right.integer);
        } else {
            left = make_int(left.integer - right.integer);
        }
    }
    return left;
}

// AI (ChatGPT), prompt: "How should the parser handle large files and absolute positions?"
// Reading once with geometric growth avoids repeatedly copying an ever-larger buffer.
static int evaluate_file(const char *input_path, const char *output_path)
{
    FILE *in = fopen(input_path, "rb");
    FILE *out;
    char *data = NULL;
    size_t length = 0;
    size_t capacity = 8192;
    size_t got;
    size_t line_start = 0;
    size_t first_error = 0;

    if (in == NULL) {
        fprintf(stderr, "calc: cannot open input file: %s\n", input_path);
        return 0;
    }

    data = (char *)malloc(capacity);
    if (data == NULL) {
        fprintf(stderr, "calc: out of memory\n");
        fclose(in);
        return 0;
    }

    /* Grow geometrically: each byte is copied only a bounded number of times. */
    while ((got = fread(data + length, 1, capacity - length, in)) > 0) {
        length += got;
        if (length == capacity) {
            size_t new_capacity = capacity * 2;
            char *larger = (char *)realloc(data, new_capacity);
            if (larger == NULL) {
                fprintf(stderr, "calc: out of memory\n");
                free(data);
                fclose(in);
                return 0;
            }
            data = larger;
            capacity = new_capacity;
        }
    }
    if (ferror(in)) {
        fprintf(stderr, "calc: cannot read input file: %s\n", input_path);
        free(data);
        fclose(in);
        return 0;
    }
    fclose(in);

    out = fopen(output_path, "w");
    if (out == NULL) {
        fprintf(stderr, "calc: cannot write output file: %s\n", output_path);
        free(data);
        return 0;
    }

    while (line_start < length) {
        size_t line_end = line_start;
        size_t first = line_start;
        Parser parser;
        Value result;

        while (line_end < length && data[line_end] != '\n') {
            line_end++;
        }
        while (first < line_end &&
               (data[first] == ' ' || data[first] == '\t' ||
                data[first] == '\r')) {
            first++;
        }

        if (first < line_end && data[first] != '#') {
            parser.text = data;
            parser.pos = line_start;
            parser.end = line_end;
            parser.file_length = length;
            parser.error_pos = 0;
            parser.failed = 0;

            result = parse_expr(&parser);
            if (!parser.failed) {
                skip_spaces(&parser);
                if (parser.pos < parser.end) {
                    fail_at(&parser, parser.pos + 1);
                }
            }
            if (parser.failed) {
                first_error = parser.error_pos;
                break;
            }

            if (result.is_float) {
                if (!isfinite(result.real)) {
                    first_error = line_start + 1;
                    break;
                }
                if (fprintf(out, "%.15g\n", result.real) < 0) {
                    fprintf(stderr, "calc: cannot write output file: %s\n",
                            output_path);
                    fclose(out);
                    free(data);
                    return 0;
                }
            } else if (fprintf(out, "%lld\n", result.integer) < 0) {
                fprintf(stderr, "calc: cannot write output file: %s\n",
                        output_path);
                fclose(out);
                free(data);
                return 0;
            }
        }

        line_start = (line_end < length) ? line_end + 1 : length;
    }

    /* No partial results are retained when any expression is invalid. */
    if (first_error != 0) {
        if (fclose(out) != 0) {
            fprintf(stderr, "calc: cannot finish output file: %s\n", output_path);
            free(data);
            return 0;
        }
        out = fopen(output_path, "w");
        if (out == NULL) {
            fprintf(stderr, "calc: cannot rewrite output file: %s\n", output_path);
            free(data);
            return 0;
        }
        if (fprintf(out, "ERROR:%zu\n", first_error) < 0) {
            fprintf(stderr, "calc: cannot write output file: %s\n", output_path);
            fclose(out);
            free(data);
            return 0;
        }
    }

    if (fclose(out) != 0) {
        fprintf(stderr, "calc: cannot finish output file: %s\n", output_path);
        free(data);
        return 0;
    }
    free(data);
    return 1;
}

static int is_txt_file(const char *name)
{
    size_t n = strlen(name);
    return n >= 4 && strcmp(name + n - 4, ".txt") == 0;
}

static char *base_name_without_txt(const char *path)
{
    const char *slash = strrchr(path, '/');
    const char *base = slash == NULL ? path : slash + 1;
    size_t n = strlen(base);
    char *result;

    if (n >= 4 && strcmp(base + n - 4, ".txt") == 0) {
        n -= 4;
    }
    result = (char *)malloc(n + 1);
    if (result != NULL) {
        memcpy(result, base, n);
        result[n] = '\0';
    }
    return result;
}

static char *make_output_path(const char *outdir, const char *base)
{
    size_t n = strlen(outdir) + strlen(base) + strlen(first_name) +
               strlen(last_name) + strlen(student_id) + 9;
    char *path = (char *)malloc(n);
    if (path != NULL) {
        (void)snprintf(path, n, "%s/%s_%s_%s_%s.txt",
                       outdir, base, first_name, last_name, student_id);
    }
    return path;
}

static int ensure_directory(const char *path)
{
    struct stat info;
    if (stat(path, &info) == 0) {
        if (S_ISDIR(info.st_mode)) {
            return 1;
        }
        fprintf(stderr, "calc: output path is not a directory: %s\n", path);
        return 0;
    }
    if (mkdir(path, 0777) != 0) {
        fprintf(stderr, "calc: cannot create output directory: %s\n", path);
        return 0;
    }
    return 1;
}

static int process_one(const char *input, const char *outdir)
{
    char *base = base_name_without_txt(input);
    char *output;
    int ok;

    if (base == NULL) {
        fprintf(stderr, "calc: out of memory\n");
        return 0;
    }
    output = make_output_path(outdir, base);
    free(base);
    if (output == NULL) {
        fprintf(stderr, "calc: out of memory\n");
        return 0;
    }
    ok = evaluate_file(input, output);
    free(output);
    return ok;
}

// AI (ChatGPT), prompt: "Implement the required -d/-o command-line behavior and output names."
// Keep CLI validation separate from expression parsing so usage errors get non-zero exits.
int main(int argc, char **argv)
{
    const char *input = NULL;
    const char *directory = NULL;
    const char *out_option = NULL;
    char *default_dir = NULL;
    int i;
    int ok = 1;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "--dir") == 0) {
            if (directory != NULL || input != NULL || i + 1 >= argc) {
                fprintf(stderr, "calc: invalid directory arguments\n");
                return 1;
            }
            directory = argv[++i];
        } else if (strcmp(argv[i], "-o") == 0 ||
                   strcmp(argv[i], "--output-dir") == 0) {
            if (out_option != NULL || i + 1 >= argc) {
                fprintf(stderr, "calc: invalid output directory arguments\n");
                return 1;
            }
            out_option = argv[++i];
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "calc: unknown option: %s\n", argv[i]);
            return 1;
        } else {
            if (input != NULL || directory != NULL) {
                fprintf(stderr, "calc: provide either one input file or -d DIR\n");
                return 1;
            }
            input = argv[i];
        }
    }

    if ((input == NULL && directory == NULL) ||
        (input != NULL && directory != NULL)) {
        fprintf(stderr, "usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
        return 1;
    }

    if (out_option != NULL) {
        if (!ensure_directory(out_option)) {
            return 1;
        }
    } else if (input != NULL) {
        char *base = base_name_without_txt(input);
        size_t n;
        if (base == NULL) {
            fprintf(stderr, "calc: out of memory\n");
            return 1;
        }
        n = strlen(base) + strlen(first_name) + strlen(student_id) + 3;
        default_dir = (char *)malloc(n);
        if (default_dir == NULL) {
            free(base);
            fprintf(stderr, "calc: out of memory\n");
            return 1;
        }
        (void)snprintf(default_dir, n, "%s_%s_%s", base, first_name, student_id);
        free(base);
        if (!ensure_directory(default_dir)) {
            free(default_dir);
            return 1;
        }
        out_option = default_dir;
    } else {
        const char *slash = strrchr(directory, '/');
        const char *base = slash == NULL ? directory : slash + 1;
        size_t n = strlen(base) + strlen(first_name) + strlen(student_id) + 3;
        default_dir = (char *)malloc(n);
        if (default_dir == NULL) {
            fprintf(stderr, "calc: out of memory\n");
            return 1;
        }
        (void)snprintf(default_dir, n, "%s_%s_%s", base, first_name, student_id);
        if (!ensure_directory(default_dir)) {
            free(default_dir);
            return 1;
        }
        out_option = default_dir;
    }

    if (input != NULL) {
        ok = process_one(input, out_option);
    } else {
        DIR *dir = opendir(directory);
        struct dirent *entry;
        if (dir == NULL) {
            fprintf(stderr, "calc: cannot open input directory: %s\n", directory);
            free(default_dir);
            return 1;
        }
        while ((entry = readdir(dir)) != NULL) {
            char *path;
            size_t n;
            struct stat info;
            if (!is_txt_file(entry->d_name)) {
                continue;
            }
            n = strlen(directory) + strlen(entry->d_name) + 2;
            path = (char *)malloc(n);
            if (path == NULL) {
                fprintf(stderr, "calc: out of memory\n");
                ok = 0;
                break;
            }
            (void)snprintf(path, n, "%s/%s", directory, entry->d_name);
            if (stat(path, &info) != 0 || !S_ISREG(info.st_mode)) {
                free(path);
                continue;
            }
            if (!process_one(path, out_option)) {
                ok = 0;
                free(path);
                break;
            }
            free(path);
        }
        if (closedir(dir) != 0) {
            fprintf(stderr, "calc: cannot close input directory: %s\n", directory);
            ok = 0;
        }
    }

    free(default_dir);
    return ok ? 0 : 1;
}
