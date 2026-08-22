"""Generate a static HTML report of external GitHub collaborations."""

from __future__ import annotations

import argparse
import html
import os
import sys
from datetime import datetime
from pathlib import Path
from typing import Any

import requests


API_URL = "https://api.github.com"
TIMEOUT_SECONDS = 30
PERMISSION_LEVELS = (
    ("admin", "admin"),
    ("maintain", "maintain"),
    ("write", "push"),
    ("triage", "triage"),
    ("read", "pull"),
)


class GitHubAPIError(RuntimeError):
    """An error response returned by the GitHub API."""


def github_get(
    session: requests.Session,
    path: str,
    *,
    params: dict[str, Any] | None = None,
) -> requests.Response:
    """Perform one GitHub API GET request with concise error reporting."""
    try:
        response = session.get(
            f"{API_URL}{path}", params=params, timeout=TIMEOUT_SECONDS
        )
    except requests.RequestException as exc:
        raise GitHubAPIError(f"request failed: {exc}") from exc

    if not response.ok:
        try:
            message = response.json().get("message", response.reason)
        except (requests.JSONDecodeError, AttributeError):
            message = response.reason
        raise GitHubAPIError(f"HTTP {response.status_code}: {message}")
    return response


def authenticated_user(session: requests.Session) -> str:
    """Return the login associated with the current token."""
    data = github_get(session, "/user").json()
    login = data.get("login")
    if not login:
        raise GitHubAPIError("GitHub response does not contain an authenticated login")
    return str(login)


def collaborator_repositories(session: requests.Session, user: str) -> list[dict[str, Any]]:
    """Fetch every external repository where the user is a collaborator."""
    params: dict[str, Any] | None = {
        "affiliation": "collaborator",
        "per_page": 100,
        "sort": "updated",
        "direction": "desc",
    }
    path = "/user/repos"
    repositories: list[dict[str, Any]] = []

    while path:
        response = github_get(session, path, params=params)
        data = response.json()
        if not isinstance(data, list):
            raise GitHubAPIError("GitHub repository response has an unexpected format")
        repositories.extend(
            repo
            for repo in data
            if str(repo.get("owner", {}).get("login", "")).casefold()
            != user.casefold()
        )

        next_url = response.links.get("next", {}).get("url")
        if next_url:
            path = next_url.removeprefix(API_URL)
            params = None
        else:
            path = ""

    return repositories


def highest_permission(permissions: Any) -> str:
    """Return the highest GitHub repository permission represented in a mapping."""
    if not isinstance(permissions, dict):
        return "unknown"
    for level, api_key in PERMISSION_LEVELS:
        if permissions.get(api_key) is True:
            return level
    return "unknown"


def escaped(value: Any, default: str = "") -> str:
    """Convert an API value to escaped HTML text."""
    if value is None:
        value = default
    return html.escape(str(value), quote=True)


def format_updated(value: Any) -> str:
    """Make GitHub's ISO timestamp compact while retaining escaped raw fallback."""
    if not isinstance(value, str) or not value:
        return "unknown"
    try:
        return datetime.fromisoformat(value.replace("Z", "+00:00")).strftime("%Y-%m-%d %H:%M UTC")
    except ValueError:
        return value


def repository_row(repo: dict[str, Any]) -> str:
    """Render one escaped repository table row."""
    owner = repo.get("owner") if isinstance(repo.get("owner"), dict) else {}
    permission = highest_permission(repo.get("permissions"))
    visibility = repo.get("visibility") or ("private" if repo.get("private") else "public")
    flags = [name for name in ("fork", "archived") if repo.get(name) is True]
    description = repo.get("description") or "No description"
    searchable = " ".join(
        str(value or "")
        for value in (
            repo.get("full_name"),
            owner.get("login"),
            description,
            repo.get("language"),
            permission,
            visibility,
            *flags,
            repo.get("updated_at"),
        )
    ).casefold()

    flag_html = " ".join(f'<span class="flag">{escaped(flag)}</span>' for flag in flags) or "—"
    return f"""        <tr data-search="{escaped(searchable)}" data-permission="{escaped(permission)}" data-visibility="{escaped(visibility)}">
          <td><a href="{escaped(repo.get('html_url'), '#')}" rel="noopener noreferrer">{escaped(repo.get('full_name'), 'unknown')}</a><small>{escaped(description)}</small></td>
          <td>{escaped(owner.get('login'), 'unknown')}</td>
          <td>{escaped(repo.get('language'), '—')}</td>
          <td><span class="badge permission-{escaped(permission)}">{escaped(permission)}</span></td>
          <td>{escaped(visibility)}</td>
          <td>{flag_html}</td>
          <td><time datetime="{escaped(repo.get('updated_at'))}">{escaped(format_updated(repo.get('updated_at')))}</time></td>
        </tr>"""


def build_html(user: str, repositories: list[dict[str, Any]]) -> str:
    """Build a standalone report, escaping all GitHub-provided strings."""
    rows = "\n".join(repository_row(repo) for repo in repositories)
    empty_hidden = " hidden" if repositories else ""
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>GitHub collaborations — {escaped(user)}</title>
  <style>
    :root {{ color-scheme: light dark; --bg:#f6f8fa; --panel:#fff; --text:#1f2328; --muted:#656d76; --border:#d0d7de; --accent:#0969da; --button:#f6f8fa; }}
    @media (prefers-color-scheme: dark) {{ :root {{ --bg:#0d1117; --panel:#161b22; --text:#e6edf3; --muted:#8d96a0; --border:#30363d; --accent:#58a6ff; --button:#21262d; }} }}
    * {{ box-sizing:border-box; }} body {{ margin:0; background:var(--bg); color:var(--text); font:14px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",sans-serif; }}
    main {{ width:min(1400px, calc(100% - 32px)); margin:36px auto; }} h1 {{ margin:0; font-size:26px; }} .summary {{ color:var(--muted); margin:4px 0 22px; }}
    .controls {{ display:flex; flex-wrap:wrap; gap:10px; margin-bottom:16px; }} input {{ flex:1 1 260px; min-width:0; padding:8px 12px; color:var(--text); background:var(--panel); border:1px solid var(--border); border-radius:6px; }}
    .filters {{ display:flex; flex-wrap:wrap; gap:6px; }} button {{ padding:7px 11px; color:var(--text); background:var(--button); border:1px solid var(--border); border-radius:6px; cursor:pointer; }} button.active {{ color:white; background:#1f883d; border-color:#1f883d; }}
    .table-wrap {{ overflow-x:auto; background:var(--panel); border:1px solid var(--border); border-radius:6px; }} table {{ width:100%; border-collapse:collapse; }} th,td {{ padding:10px 12px; text-align:left; vertical-align:top; border-bottom:1px solid var(--border); }} th {{ white-space:nowrap; background:var(--button); }} tr:last-child td {{ border-bottom:0; }}
    a {{ color:var(--accent); font-weight:600; text-decoration:none; }} a:hover {{ text-decoration:underline; }} small {{ display:block; margin-top:2px; min-width:260px; color:var(--muted); font-weight:400; }} .badge,.flag {{ display:inline-block; padding:1px 7px; border:1px solid var(--border); border-radius:12px; white-space:nowrap; }} .flag {{ margin:0 3px 3px 0; }}
    .empty {{ padding:28px; text-align:center; color:var(--muted); }} [hidden] {{ display:none !important; }}
    @media (max-width:700px) {{ main {{ width:calc(100% - 20px); margin:20px auto; }} th,td {{ padding:8px; }} }}
  </style>
</head>
<body>
  <main>
    <h1>External GitHub collaborations</h1>
    <p class="summary">Authenticated as <strong>{escaped(user)}</strong> · <span id="visible-count">{len(repositories)}</span> of {len(repositories)} repositories</p>
    <div class="controls">
      <input id="search" type="search" placeholder="Search repositories…" aria-label="Search repositories">
      <div class="filters" id="permission-filters" aria-label="Permission filter">
        {''.join(f'<button type="button" data-value="{p}" class="{"active" if p == "all" else ""}">{p}</button>' for p in ('all', 'read', 'triage', 'write', 'maintain', 'admin'))}
      </div>
      <div class="filters" id="visibility-filters" aria-label="Visibility filter">
        {''.join(f'<button type="button" data-value="{v}" class="{"active" if v == "all" else ""}">{v}</button>' for v in ('all', 'public', 'private'))}
      </div>
    </div>
    <div class="table-wrap">
      <table>
        <thead><tr><th>Repository</th><th>Owner</th><th>Language</th><th>Permission</th><th>Visibility</th><th>Flags</th><th>Updated</th></tr></thead>
        <tbody id="repositories">
{rows}
        </tbody>
      </table>
      <div id="empty" class="empty"{empty_hidden}>No repositories match the current filters.</div>
    </div>
  </main>
  <script>
    (() => {{
      const search = document.getElementById('search');
      const rows = [...document.querySelectorAll('#repositories tr')];
      const count = document.getElementById('visible-count');
      const empty = document.getElementById('empty');
      let permission = 'all', visibility = 'all';
      function applyFilters() {{
        const query = search.value.trim().toLocaleLowerCase(); let visible = 0;
        rows.forEach(row => {{
          const show = (!query || row.dataset.search.includes(query)) && (permission === 'all' || row.dataset.permission === permission) && (visibility === 'all' || row.dataset.visibility === visibility);
          row.hidden = !show; if (show) visible += 1;
        }});
        count.textContent = visible; empty.hidden = visible !== 0;
      }}
      function bindFilter(id, setter) {{
        document.getElementById(id).addEventListener('click', event => {{
          const button = event.target.closest('button'); if (!button) return;
          event.currentTarget.querySelectorAll('button').forEach(item => item.classList.toggle('active', item === button));
          setter(button.dataset.value); applyFilters();
        }});
      }}
      search.addEventListener('input', applyFilters);
      bindFilter('permission-filters', value => permission = value);
      bindFilter('visibility-filters', value => visibility = value);
    }})();
  </script>
</body>
</html>
"""


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "-o", "--output", type=Path, default=Path("github-repositories.html"),
        help="output HTML file (default: github-repositories.html)",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    token = os.environ.get("GITHUB_TOKEN")
    if not token:
        print("Error: GITHUB_TOKEN environment variable is not set", file=sys.stderr)
        return 1

    session = requests.Session()
    session.headers.update({
        "Accept": "application/vnd.github+json",
        "Authorization": f"Bearer {token}",
        "X-GitHub-Api-Version": "2022-11-28",
        "User-Agent": "github-collaborations-report",
    })
    try:
        user = authenticated_user(session)
        repositories = collaborator_repositories(session, user)
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(build_html(user, repositories), encoding="utf-8")
    except GitHubAPIError as exc:
        print(f"Error: {exc}", file=sys.stderr)
        return 1
    except OSError as exc:
        print(f"Error: cannot write {args.output}: {exc}", file=sys.stderr)
        return 1
    finally:
        session.close()

    print(f"Generated {args.output} with {len(repositories)} repositories.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
