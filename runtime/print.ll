
define %struct.pyret-value @print(%struct.pyret-value %obj) {
    %type-tag = extractvalue %struct.pyret-value %obj, 0
    switch i32 %type-tag, label %default-branch [ i32 0, label %print-number ]

print-number:
    call void @print-pyret-value(%struct.pyret-value %obj)
    br label %exit

default-branch:
    ;; Don't do anything for now.
    br label %exit

exit:
    ret %struct.pyret-value %obj
}
