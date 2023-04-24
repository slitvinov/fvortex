```
awk '{print "      write(*, '\''(A)'\'') " "'\''" $0 "'\''"}' p.xdmf2  | awk 'length($0) > 72 {print length($0), $0}'
```