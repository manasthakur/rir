localVar <- 42
f1 <- function(f) {
  localVar <- "local"
  f()
  localVar
}

runTests <- function(shouldRestart) {
  runCount <- 0
  rir.freeze({
    runCount <- runCount + 1

    rir.compile(function()
        for (i in 1:400) f(function()1)
    )()
    print("PIR before fail")
    rir.disassemble(f)
    stopifnot("local" == f(function() 12))
    f(evilFun)
    if (shouldRestart)
      stopifnot(42 == f(evilFun))
    else
      stopifnot("local" == f(evilFun))
    rir.compile(function()
        for (i in 1:400) f(function()1)
    )()
    print("PIR after fail")
    rir.disassemble(f)
    if (shouldRestart)
      stopifnot(42 == f(evilFun))
    else
      stopifnot("local" == f(evilFun))
  })
  # stopifnot(shouldRestart == (runCount >= 2))
}

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() print("ok")

runTests(FALSE)

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() rm("localVar", envir=sys.frame(-1))

runTests(TRUE)

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() { leak <<- sys.frame(-1); assign("localVar", 42, leak) }

runTests(TRUE)

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() { leak <<- sys.frame(-1); leak$localVar <- 42 }

runTests(TRUE)

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() stopifnot(sys.frame(-1)$localVar == "local")

runTests(FALSE)

f <- function(g) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(g)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() rm("localVar", envir=sys.frame(-2))

runTests(TRUE)

f <- function(g) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(g)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() { leak <<- sys.frame(-2); assign("localVar", 42, leak) }

runTests(TRUE)

f <- function(g) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(g)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() { leak <<- sys.frame(-2); leak$localVar <- 42 }

runTests(TRUE)

f <- function(g) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(g)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() stopifnot(sys.frame(-2)$localVar == "local")

runTests(FALSE)