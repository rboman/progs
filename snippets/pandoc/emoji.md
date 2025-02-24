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

Le but est d'avoir des emojis dans le pdf.
Pour en avoir dans Markdown ou HTML, ça ne pose pas de problème

Pour la conversion pdf, on rencontre le problème que latex ne supporte pas l'UTF8. Il faut utiliser lualatex ou xelatex et spécifier des polices.




Build this file with the following command:
```
pandoc --pdf-engine=lualatex emoji.md -o emoji.pdf
```

(testé février 2025 sous ubuntu 24.04)

It 😺 lorem 👅 \LaTeX.

```julia
🟡 = 2
```

Autre possibilité pas testée: 

[masbicudo/Pandoc-Emojis-Filter: Pandoc filter to convert unicode emoji characters into images. This filter specifically targets the conversion from Markdown to PDF at this moment.](https://github.com/masbicudo/Pandoc-Emojis-Filter)
