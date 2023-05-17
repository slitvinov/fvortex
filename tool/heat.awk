BEGIN {
    me = "heat.awk"
    getline
    if ("variables=x,y,w" != $0) {
        printf "%s: input file format is not supported\n", me | "cat >&2"
        exit(1)
    }
    getline
    sub(/zone[ \t]*/, "")
    split($0, a, ",")
    sub(/[ \t]*i=[ \t]*/, "", a[1])
    sub(/[ \t]*j=[ \t]*/, "", a[2])

    m = a[1]
    n = a[2]
    if (m !~ /[0-9]+/) {
        printf "%s: input file format is not supported\n", me | "cat >&2"
        exit(1)
    }
    if (n !~ /[0-9]+/) {
        printf "%s: input file format is not supported\n", me | "cat >&2"
        exit(1)
    }
    m += 0
    n += 0
    First = 1
    for (i = 0; i < m; i++)
        for (j = 0; j < n; j++) {
            if (getline != 1) {
                printf "%s: error parsing file\n", me | "cat >&2"
                exit(1)
            }
            k = i * n + j
            w[k] = $3
            if (First) {
                ma = w[k]
                mi = w[k]
                First = 0
            } else {
                if (w[k]  > ma)
                    ma = w[k]
                if (w[k] < mi)
                    mi = w[k]
            }
        }
    M = 2^16 - 1
    print "P3"
    printf "%d %d\n", m, n
    print M
    for (j = n - 1; j >= 0; j--)
        for (i = 0; i < m; i++) {
            k = i * n + j
            jet(w[k], mi, ma)
            print(int(M*R), int(M*G), int(M*B))
        }
}

function jet(v, l, h) {
    if (v < l)
        v = l
    if (v > h)
        v = h
    if (l != h)
        v = 4 * (v - l) / (h - l)
    else
        v = 0

    R = 0
    G = B = 1
    if (v < 1)
        G = v
    else if (v < 2)
        B = 2 - v
    else if (v < 3) {
        R = v - 2
        B = 0
    } else {
        R = 1
        G = 4 - v;
        B = 0
    }
}
