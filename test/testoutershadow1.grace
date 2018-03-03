class foo {
}

class bar {
    inherit foo
    method x { "X" }
    class inner {
        method x { "XXXXX" }
    }
}

print(bar.inner.x)

