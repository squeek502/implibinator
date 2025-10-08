Module-Definition (.def) to Import Library (.lib) conversion (note: specifically MinGW .def files, at least for now).

Intended to be upstreamed into the Zig compiler to close [#17807: ability to create import libs from def files without LLVM](https://github.com/ziglang/zig/issues/17807).

> [!NOTE]
> This implementation [has been merged into Zig](https://github.com/ziglang/zig/pull/25414) and therefore this repository is now dormant. This repository may still see development in the future, though, since the infrastructure for exhaustive testing still exclusively exists here.

Useful links:
- https://learn.microsoft.com/en-us/cpp/build/reference/module-definition-dot-def-files?view=msvc-170
- https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#archive-library-file-format
- https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#import-library-format
- https://github.com/llvm/llvm-project/blob/main/llvm/lib/Object/COFFImportFile.cpp
- https://github.com/llvm/llvm-project/blob/main/llvm/lib/Object/ArchiveWriter.cpp
- https://github.com/llvm/llvm-project/blob/main/llvm/lib/Object/COFFModuleDefinition.cpp
