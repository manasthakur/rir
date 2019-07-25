n <- rir.compile(sys.function)
f <- function(x) x
f({ n(); })
f({ n(); })
f({ n(); })
f({ n(); })

hang <- function() {
    j <- 1
    for (i in 1:3) {
        while (j < i)
            j <- j + 1
        print(i)
    }
}

hang()
hang()
hang()
hang()
