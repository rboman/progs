# -*- coding: utf-8 -*-
#
#   Copyright 2017-2021 Romain Boman
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

"""
Management of SVN and git commands
"""

# note: lancer les tests écrits par Codex:
# python -m unittest tests.test_versioning tests.test_updateoffi

import pytools.utils as pu
import os
import os.path
import subprocess
import re
import shlex
import sys
from pathlib import Path
from typing import Sequence
from urllib.parse import urlparse


STRICT_SSH_OPTIONS = [
    "-o", "BatchMode=yes",
    "-o", "IdentitiesOnly=yes",
    "-o", "ConnectionAttempts=1",
    "-o", "ConnectTimeout=10",
    "-o", "NumberOfPasswordPrompts=0",
    "-o", "PreferredAuthentications=publickey",
    "-o", "PasswordAuthentication=no",
    "-o", "KbdInteractiveAuthentication=no",
]

GIT_SSH_COMMAND = " ".join(["ssh", *STRICT_SSH_OPTIONS])

SSH_ERROR_PATTERNS = [
    "Permission denied (publickey)",
    "Could not resolve hostname",
    "Connection timed out",
    "Connection refused",
    "Connection reset",
    "Connection closed",
    "Host key verification failed",
    "kex_exchange_identification",
    "Too many authentication failures",
    "remote end hung up unexpectedly",
    "fatal:",
    "Could not read from remote repository",
]


class GitCommandError(RuntimeError):
    """Raised when a git or SSH diagnostic command fails."""


def _format_command(cmd: Sequence[str]) -> str:
    if os.name == "nt":
        return subprocess.list2cmdline([str(part) for part in cmd])
    return shlex.join([str(part) for part in cmd])


def _git_env() -> dict[str, str]:
    env = os.environ.copy()
    env["GIT_SSH_COMMAND"] = GIT_SSH_COMMAND
    return env


def _looks_like_ssh_error(stdout: str, stderr: str) -> bool:
    output = f"{stdout}\n{stderr}".casefold()
    return any(pattern.casefold() in output for pattern in SSH_ERROR_PATTERNS)


def _ssh_target_from_git_url(repo_url: str) -> str | None:
    if re.match(r"^[A-Za-z]:[\\/]", repo_url):
        return None

    parsed = urlparse(repo_url)
    if parsed.scheme == "ssh" and parsed.hostname:
        if parsed.username:
            return f"{parsed.username}@{parsed.hostname}"
        return parsed.hostname

    match = re.match(r"(?:(?P<user>[^@:/]+)@)?(?P<host>[^:/]+):.+", repo_url)
    if match:
        user = match.group("user")
        host = match.group("host")
        if user:
            return f"{user}@{host}"
        return host

    return None


def _ssh_failure_hint(ssh_check_target: str | None) -> list[str]:
    lines = [
        "SSH authentication or connectivity failed.",
        "No further repository will be contacted.",
    ]
    if ssh_check_target:
        lines.extend([
            "Check the SSH configuration with:",
            f"    ssh -vvv -T {ssh_check_target}",
        ])
    else:
        lines.extend([
            "Check the SSH configuration for this repository's remote host.",
        ])
    return lines


def _format_failure_diagnostic(
        *,
        repo_name: str,
        cwd: Path,
        description: str,
        cmd: Sequence[str],
        returncode: int | None,
        stdout: str = "",
        stderr: str = "",
        ssh_check_target: str | None = None,
        extra_hint: str | None = None) -> str:
    lines = [
        "Git command failed.",
        f"Repository: {repo_name}",
        f"Working directory: {cwd}",
        f"Operation: {description}",
        f"Command: {_format_command(cmd)}",
        f"Return code: {returncode}",
    ]
    if stdout:
        lines.extend(["stdout:", stdout.rstrip()])
    if stderr:
        lines.extend(["stderr:", stderr.rstrip()])
    if extra_hint:
        lines.extend(["Interpretation:", extra_hint])
    elif _looks_like_ssh_error(stdout, stderr):
        lines.append("Interpretation:")
        lines.extend(_ssh_failure_hint(ssh_check_target))
    return "\n".join(lines)


def run_git_command(
        cmd: Sequence[str],
        *,
        cwd: os.PathLike[str] | str,
        description: str,
        repo_name: str = "<unknown>",
        ssh_check_target: str | None = None,
        verbose: bool = False,
        show_output: bool = False) -> subprocess.CompletedProcess[str]:
    """Run one git command with strict SSH settings and clear diagnostics."""

    cwd_path = Path(cwd)
    if verbose:
        print(f"[git:{repo_name}] cwd={cwd_path} $ {_format_command(cmd)}")
    try:
        result = subprocess.run(
            [str(part) for part in cmd],
            cwd=cwd_path,
            env=_git_env(),
            shell=False,
            text=True,
            capture_output=True,
        )
    except OSError as exc:
        diagnostic = _format_failure_diagnostic(
            repo_name=repo_name,
            cwd=cwd_path,
            description=description,
            cmd=cmd,
            returncode=None,
            stderr=str(exc),
            ssh_check_target=ssh_check_target,
            extra_hint="The command could not be started. Check that git is installed and available in PATH.",
        )
        print(diagnostic, file=sys.stderr)
        raise GitCommandError(diagnostic) from exc
    if result.returncode == 0:
        if show_output:
            if result.stdout:
                print(result.stdout, end="")
            if result.stderr:
                print(result.stderr, end="", file=sys.stderr)
        return result

    diagnostic = _format_failure_diagnostic(
        repo_name=repo_name,
        cwd=cwd_path,
        description=description,
        cmd=cmd,
        returncode=result.returncode,
        stdout=result.stdout,
        stderr=result.stderr,
        ssh_check_target=ssh_check_target,
    )
    print(diagnostic, file=sys.stderr)
    raise GitCommandError(diagnostic)


def _raise_git_diagnostic(
        *,
        repo_name: str,
        cwd: os.PathLike[str] | str,
        description: str,
        cmd: Sequence[str],
        stdout: str = "",
        stderr: str = "",
        extra_hint: str | None = None) -> None:
    diagnostic = _format_failure_diagnostic(
        repo_name=repo_name,
        cwd=Path(cwd),
        description=description,
        cmd=cmd,
        returncode=None,
        stdout=stdout,
        stderr=stderr,
        extra_hint=extra_hint,
    )
    print(diagnostic, file=sys.stderr)
    raise GitCommandError(diagnostic)


def check_ssh_access(host: str, user: str = "git") -> subprocess.CompletedProcess[str]:
    """Check SSH access to one git host once, without retrying."""

    ssh_target = f"{user}@{host}" if user else host
    cmd = ["ssh", *STRICT_SSH_OPTIONS, "-T", ssh_target]
    cwd_path = Path.cwd()
    try:
        result = subprocess.run(
            cmd,
            cwd=cwd_path,
            shell=False,
            text=True,
            capture_output=True,
        )
    except OSError as exc:
        diagnostic = _format_failure_diagnostic(
            repo_name=ssh_target,
            cwd=cwd_path,
            description="check SSH access",
            cmd=cmd,
            returncode=None,
            stderr=str(exc),
            ssh_check_target=ssh_target,
            extra_hint="The command could not be started. Check that ssh is installed and available in PATH.",
        )
        print(diagnostic, file=sys.stderr)
        raise GitCommandError(diagnostic) from exc
    output = f"{result.stdout}\n{result.stderr}"
    if result.returncode == 0 or (result.returncode == 1 and "Welcome to" in output):
        return result

    diagnostic = _format_failure_diagnostic(
        repo_name=ssh_target,
        cwd=cwd_path,
        description="check SSH access",
        cmd=cmd,
        returncode=result.returncode,
        stdout=result.stdout,
        stderr=result.stderr,
        ssh_check_target=ssh_target,
    )
    print(diagnostic, file=sys.stderr)
    raise GitCommandError(diagnostic)


class Repo:
    def __init__(self):
        pass

    def update(self):
        pass


class GITRepo(Repo):
    """ a git repository
    """
    def __init__(self, name, repo, verbose=False):
        self.name = name
        self.repo = repo
        self.verbose = verbose
        self.ssh_check_target = _ssh_target_from_git_url(repo)

    def update(self):
        repo_path = Path(self.name)

        if not repo_path.is_dir():
            cmd = ['git', 'clone', '--recursive', self.repo, self.name]
            run_git_command(
                cmd,
                cwd=Path.cwd(),
                description="clone repository",
                repo_name=self.name,
                ssh_check_target=self.ssh_check_target,
                verbose=self.verbose,
                show_output=True,
            )

        else:
            cmd = ['git', 'pull']
            run_git_command(
                cmd,
                cwd=repo_path,
                description="pull repository",
                repo_name=self.name,
                ssh_check_target=self.ssh_check_target,
                verbose=self.verbose,
                show_output=True,
            )

            # update submodules
            if (repo_path / '.gitmodules').is_file():
                # '--init' for the case where modules were not present before
                cmd = ['git', 'submodule', 'update', '--init'] # '--recursive'] if sub-sub-modules
                run_git_command(
                    cmd,
                    cwd=repo_path,
                    description="update git submodules",
                    repo_name=self.name,
                    ssh_check_target=self.ssh_check_target,
                    verbose=self.verbose,
                    show_output=True,
                )

        # set 'core.filemode=false' in '.git/config' on windows!
        # (otherwise executable files are considered as diffs)
        if not pu.isUnix():
            cmd = ['git', 'config', 'core.filemode', 'false']
            run_git_command(
                cmd,
                cwd=repo_path,
                description="configure core.filemode",
                repo_name=self.name,
                ssh_check_target=self.ssh_check_target,
                verbose=self.verbose,
            )

    def outdated(self):
        """checks whether the working copy is outdated or not
        """
        repo_path = Path(self.name)

        if not repo_path.is_dir():
            return True

        # fetch everything
        cmd = ['git', 'remote', '-v', 'update']
        run_git_command(
            cmd,
            cwd=repo_path,
            description="fetch remote repository state",
            repo_name=self.name,
            ssh_check_target=self.ssh_check_target,
            verbose=self.verbose,
        )

        cmd = ['git', 'status', '--porcelain=v2', '--branch', '--untracked-files=no']
        result = run_git_command(
            cmd,
            cwd=repo_path,
            description="read repository status",
            repo_name=self.name,
            ssh_check_target=self.ssh_check_target,
            verbose=self.verbose,
        )

        for line in result.stdout.splitlines():
            if line.startswith("# branch.ab "):
                match = re.match(r"# branch\.ab \+(-?\d+) -(-?\d+)$", line)
                if not match:
                    _raise_git_diagnostic(
                        repo_name=self.name,
                        cwd=repo_path,
                        description="parse repository status",
                        cmd=cmd,
                        stdout=result.stdout,
                        stderr=result.stderr,
                        extra_hint="Cannot parse the upstream ahead/behind information reported by git status.",
                    )
                behind = int(match.group(2))
                return behind > 0

        _raise_git_diagnostic(
            repo_name=self.name,
            cwd=repo_path,
            description="read repository status",
            cmd=cmd,
            stdout=result.stdout,
            stderr=result.stderr,
            extra_hint="No upstream branch is configured for this repository, so remote freshness cannot be checked.",
        )

    def checkout(self, branch='master'):
        repo_path = Path(self.name)
        cmd = ['git', 'checkout', branch]
        run_git_command(
            cmd,
            cwd=repo_path,
            description=f"checkout branch {branch}",
            repo_name=self.name,
            ssh_check_target=self.ssh_check_target,
            verbose=self.verbose,
            show_output=True,
        )


class SVNRepo(Repo):
    """Old class used to manage SVN repositories
    """
    def __init__(self, name, repo):
        self.name = name
        self.repo = repo

        # set SVN_SSH, sinon: "can't create tunnel"
        if not pu.isUnix():
            os.environ[
                'SVN_SSH'] = r'C:\\Program Files\\TortoiseSVN\\bin\\TortoisePlink.exe'  # '\\\\' ou 'r et \\' !!

    def update(self):

        if not os.path.isdir(self.name):
            cmd = 'svn co %s %s' % (self.repo, self.name)
        else:
            cmd = 'svn update %s' % self.name

        print(cmd)
        status = subprocess.call(cmd, shell=True)
        if status:
            raise Exception('"%s" FAILED with error %d' % (cmd, status))

    def outdated(self):
        "checks whether the working copy is outdated"

        if not os.path.isdir(self.name):
            return True

        # svn info
        out = subprocess.check_output(['svn', 'info', self.name])
        out = out.decode(errors='ignore')  # python 3 returns bytes
        m = re.search(r'Last Changed Rev: (\d+)', out)
        if m and len(m.groups()) > 0:
            version = m.group(1)
        else:
            raise Exception('cannot read "svn info" output')

        # svn info -r HEAD
        out = subprocess.check_output(['svn', 'info', '-r', 'HEAD', self.name])
        out = out.decode(errors='ignore')  # python 3 returns bytes
        m = re.search(r'Last Changed Rev: (\d+)', out)
        if m and len(m.groups()) > 0:
            version_HEAD = m.group(1)
        else:
            raise Exception('cannot read "svn info -r HEAD" output')

        #print('version =', version)
        #print('version_HEAD =', version_HEAD)

        return version != version_HEAD
