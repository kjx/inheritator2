class foo {
    method x { "X" }
}

class bar {
    inherit foo
    class inner {
        method x { "XXXXX" }
    }
}

print(bar.inner.x)

