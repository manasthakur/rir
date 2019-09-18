# Only run with RIR_REFLECT_GUARD=retry
if (Sys.getenv("RIR_REFLECT_GUARD", unset="off") != "retry")
  exit()

expectExit <- function(code) {
  code
  stop("should have exited")
}

f1 <- function(f) {
  localVar <- "local"
  f()
  localVar
}

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() rm("localVar", envir=sys.frame(-1))

## Main test code (we need to copy/paste because retry assertion failures ignore tryCatch)
##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
## 

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() { leak <<- sys.frame(-1); assign("localVar", 42, leak) }

##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
## 

f <- function(f) {
  localVar <- "local"
  f()
  localVar
}
evilFun <- function() { leak <<- sys.frame(-1); leak$localVar <- 42 }

##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
##

f <- function(f) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(f)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() rm("localVar", envir=sys.frame(-2))

##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
## 

f <- function(f) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(f)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() { leak <<- sys.frame(-2); assign("localVar", 42, leak) }

##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
## 

f <- function(f) {
  localVar <- "local"
  polymorphic <- data.frame()
  a <- 1
  f1(f)
  polymorphic <- polymorphic + a
  localVar
}
evilFun <- function() { leak <<- sys.frame(-2); leak$localVar <- 42 }

##
rir.compile(function()
    for (i in 1:400) f(function()1)
)()
# PIR before fail
rir.disassemble(f)

localVar <- 42
stopifnot("local" == f(function() 12))
expectExit(f(evilFun))
stopifnot(42 == f(evilFun))

rir.compile(function()
    for (i in 1:400) f(function()1)
)()
print("  PIR after fail")
rir.disassemble(f)
stopifnot(42 == f(evilFun))
##