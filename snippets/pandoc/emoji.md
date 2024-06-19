---
header-includes:
- |
  ```{=latex}
  \directlua{luaotfload.add_fallback(
               "myfallback",
               {"NotoColorEmoji:mode=harf;"}
             )}
  \setmainfont[RawFeature={fallback=myfallback}]{LatinModernRoman}
  \setmonofont[RawFeature={fallback=myfallback}]{LatinModernMono}
  ```
---

Build this file with the following command:
```
pandoc --pdf-engine=lualatex emoji.md -o emoji.pdf
```



It 😺 lorem 👅 \LaTeX.

```julia
🟡 = 2
```


