# --- coding: utf-8 -*-
# Version améliorée avec streaming et sauvegarde en markdown

from pathlib import Path
from openai import OpenAI
import sys
import time
import argparse
from datetime import datetime


MODEL = "qwen3-4b-instruct-special"

client = OpenAI(
    base_url="http://localhost:1234/v1",
    api_key="lm-studio",
)


def build_prompt(article_text: str) -> str:
    return f"""
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
{article_text}
---
"""


def summarize_article_stream(path: Path) -> tuple[str, float]:
    text = path.read_text(encoding="utf-8")

    prompt = build_prompt(text)

    start = time.perf_counter()
    chunks: list[str] = []

    stream = client.chat.completions.create(
        model=MODEL,
        messages=[
            {
                "role": "system",
                "content": (
                    "Tu es un assistant spécialisé dans la synthèse fidèle "
                    "d’articles de presse."
                ),
            },
            {"role": "user", "content": prompt},
        ],
        temperature=0.5,
        max_tokens=1200,
        stream=True,
    )

    print()

    for event in stream:
        delta = event.choices[0].delta.content

        if delta:
            print(delta, end="", flush=True)
            chunks.append(delta)

    elapsed = time.perf_counter() - start

    print()
    print()
    print("---")
    print(f"Temps total : {elapsed:.2f} s")

    summary = "".join(chunks).strip()

    print(f"Caractères générés : {len(summary)}")

    return summary, elapsed


def save_markdown(
    output_path: Path,
    source_path: Path,
    summary: str,
    elapsed: float,
) -> None:
    now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    markdown = f"""# Résumé d'article

## Source

- Fichier : `{source_path.name}`

## Résumé

{summary}

---

## Métadonnées

- Modèle : `{MODEL}`
- Date : `{now}`
- Temps de génération : `{elapsed:.2f} s`
"""

    output_path.write_text(markdown, encoding="utf-8")

    print(f"\nRésumé sauvegardé dans : {output_path}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "input_file",
        help="Fichier texte à résumer",
    )

    parser.add_argument(
        "--output",
        "-o",
        help="Fichier markdown de sortie",
    )

    return parser.parse_args()


def main() -> None:
    args = parse_args()

    input_path = Path(args.input_file)

    if not input_path.exists():
        print(f"Fichier introuvable : {input_path}")
        raise SystemExit(1)

    summary, elapsed = summarize_article_stream(input_path)

    if args.output:
        output_path = Path(args.output)

        save_markdown(
            output_path=output_path,
            source_path=input_path,
            summary=summary,
            elapsed=elapsed,
        )


if __name__ == "__main__":
    main()