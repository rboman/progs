# --- coding: utf-8 -*-
# Prends un fichier texte en argument et le résume à l'aide de LM Studio.
from pathlib import Path
from openai import OpenAI
import sys


MODEL = "qwen3-4b-instruct-special"  # à adapter au nom vu dans LM Studio

client = OpenAI(
    base_url="http://localhost:1234/v1",
    api_key="lm-studio",  # valeur fictive acceptée par LM Studio
)


def summarize_article(path: Path) -> str:
    text = path.read_text(encoding="utf-8")

    prompt = f"""
Résume cet article de presse en français.

Objectif :
capturer les idées importantes et les tensions principales.

Contraintes :
- 5 à 8 points maximum
- phrases courtes
- reste fidèle au texte
- évite les généralités vagues
- ne surpondère pas la fin de l'article

Termine IMPÉRATIVEMENT par :
À retenir : une phrase qui synthétise l'idée centrale de tout l'article.

Article :
---
{text}
---
"""

    response = client.chat.completions.create(
        model=MODEL,
        messages=[
            {
                "role": "system",
                "content": "Tu es un assistant spécialisé dans la synthèse fidèle d’articles de presse.",
            },
            {"role": "user", "content": prompt},
        ],
        temperature=0.5,
        max_tokens=1200,
    )

    return response.choices[0].message.content.strip()


def main() -> None:
    if len(sys.argv) != 2:
        print("Usage: python summarize_article.py article.txt")
        raise SystemExit(1)

    path = Path(sys.argv[1])

    if not path.exists():
        print(f"Fichier introuvable : {path}")
        raise SystemExit(1)

    summary = summarize_article(path)
    print("\n" + summary + "\n")


if __name__ == "__main__":
    main()