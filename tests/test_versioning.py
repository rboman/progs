import contextlib
import io
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import pytools.versioning as versioning


def completed(cmd, returncode=0, stdout="", stderr=""):
    return subprocess.CompletedProcess(cmd, returncode, stdout=stdout, stderr=stderr)


@contextlib.contextmanager
def temporary_cwd(path):
    previous = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(previous)


class RunGitCommandTests(unittest.TestCase):
    def test_successful_command_uses_strict_ssh_environment(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch("pytools.versioning.subprocess.run") as run:
            run.return_value = completed(["git", "status"], stdout="ok\n")

            result = versioning.run_git_command(
                ["git", "status"],
                cwd=tmp,
                description="read status",
                repo_name="repo",
            )

        self.assertEqual(result.stdout, "ok\n")
        kwargs = run.call_args.kwargs
        self.assertFalse(kwargs["shell"])
        self.assertTrue(kwargs["text"])
        self.assertTrue(kwargs["capture_output"])
        ssh_command = kwargs["env"]["GIT_SSH_COMMAND"]
        self.assertIn("BatchMode=yes", ssh_command)
        self.assertIn("IdentitiesOnly=yes", ssh_command)
        self.assertIn("ConnectionAttempts=1", ssh_command)
        self.assertIn("ConnectTimeout=10", ssh_command)
        self.assertIn("NumberOfPasswordPrompts=0", ssh_command)
        self.assertIn("PreferredAuthentications=publickey", ssh_command)
        self.assertIn("PasswordAuthentication=no", ssh_command)
        self.assertIn("KbdInteractiveAuthentication=no", ssh_command)

    def test_verbose_prints_command_before_running_it(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch("pytools.versioning.subprocess.run") as run:
            out = io.StringIO()

            def fake_run(*args, **kwargs):
                self.assertIn("git pull", out.getvalue())
                return completed(["git", "pull"])

            run.side_effect = fake_run

            with contextlib.redirect_stdout(out):
                versioning.run_git_command(
                    ["git", "pull"],
                    cwd=tmp,
                    description="pull repository",
                    repo_name="repo",
                    verbose=True,
                )

        output = out.getvalue()
        self.assertIn("[git:repo]", output)
        self.assertIn("git pull", output)

    def test_permission_denied_diagnostic_includes_stdout_stderr_and_ssh_hint(self):
        stderr = "git@example.test: Permission denied (publickey).\n"
        stdout = "remote preface\n"
        with tempfile.TemporaryDirectory() as tmp, mock.patch("pytools.versioning.subprocess.run") as run:
            run.return_value = completed(["git", "fetch"], returncode=128, stdout=stdout, stderr=stderr)
            err = io.StringIO()

            with contextlib.redirect_stderr(err), self.assertRaises(versioning.GitCommandError) as ctx:
                versioning.run_git_command(
                    ["git", "fetch"],
                    cwd=tmp,
                    description="fetch remote",
                    repo_name="repo",
                )

        diagnostic = str(ctx.exception)
        self.assertIn("Repository: repo", diagnostic)
        self.assertIn("Operation: fetch remote", diagnostic)
        self.assertIn("Return code: 128", diagnostic)
        self.assertIn("stdout:", diagnostic)
        self.assertIn(stdout.rstrip(), diagnostic)
        self.assertIn("stderr:", diagnostic)
        self.assertIn(stderr.rstrip(), diagnostic)
        self.assertIn("SSH authentication or connectivity failed.", diagnostic)
        self.assertIn("No further repository will be contacted.", diagnostic)
        self.assertIn("Check the SSH configuration for this repository's remote host.", diagnostic)
        self.assertIn("Permission denied (publickey)", err.getvalue())

    def test_connection_timeout_is_reported_as_ssh_connectivity_failure(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch("pytools.versioning.subprocess.run") as run:
            run.return_value = completed(["git", "fetch"], returncode=128, stderr="ssh: connect to host example.test port 22: Connection timed out\n")

            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(versioning.GitCommandError) as ctx:
                versioning.run_git_command(
                    ["git", "fetch"],
                    cwd=tmp,
                    description="fetch remote",
                    repo_name="repo",
                )

        self.assertIn("Connection timed out", str(ctx.exception))
        self.assertIn("SSH authentication or connectivity failed.", str(ctx.exception))

    def test_git_repo_error_uses_ssh_hint_from_remote_url(self):
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run, mock.patch("pytools.versioning.pu.isUnix", return_value=True):
            Path("repo").mkdir()
            run.return_value = completed(["git", "pull"], returncode=128, stderr="Permission denied (publickey)\n")

            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(versioning.GitCommandError) as ctx:
                versioning.GITRepo("repo", "git@example.test:groupe/projet.git").update()

        self.assertIn("ssh -vvv -T git@example.test", str(ctx.exception))

    def test_cwd_is_unchanged_after_success_and_failure(self):
        with tempfile.TemporaryDirectory() as tmp, mock.patch("pytools.versioning.subprocess.run") as run:
            original_cwd = os.getcwd()
            run.return_value = completed(["git", "status"], stdout="ok\n")
            versioning.run_git_command(["git", "status"], cwd=tmp, description="status", repo_name="repo")
            self.assertEqual(os.getcwd(), original_cwd)

            run.return_value = completed(["git", "status"], returncode=1, stderr="fatal: no repository\n")
            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(versioning.GitCommandError):
                versioning.run_git_command(["git", "status"], cwd=tmp, description="status", repo_name="repo")
            self.assertEqual(os.getcwd(), original_cwd)


class GITRepoTests(unittest.TestCase):
    def test_update_stops_after_first_error_without_retry(self):
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run, mock.patch("pytools.versioning.pu.isUnix", return_value=True):
            Path("repo").mkdir()
            run.return_value = completed(["git", "pull"], returncode=128, stderr="Permission denied (publickey)\n")

            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(versioning.GitCommandError):
                versioning.GITRepo("repo", "git@example.test:groupe/projet.git").update()

        self.assertEqual(run.call_count, 1)
        self.assertEqual(run.call_args.args[0], ["git", "pull"])

    def test_outdated_returns_false_when_remote_has_no_missing_commits(self):
        status = "\n".join([
            "# branch.oid abc123",
            "# branch.head master",
            "# branch.upstream origin/master",
            "# branch.ab +0 -0",
        ])
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run:
            Path("repo").mkdir()
            run.side_effect = [
                completed(["git", "remote", "-v", "update"]),
                completed(["git", "status"], stdout=status),
            ]

            outdated = versioning.GITRepo("repo", "git@example.test:groupe/projet.git").outdated()

        self.assertFalse(outdated)
        self.assertEqual(run.call_count, 2)

    def test_outdated_returns_true_when_remote_has_missing_commits(self):
        status = "\n".join([
            "# branch.oid abc123",
            "# branch.head master",
            "# branch.upstream origin/master",
            "# branch.ab +0 -3",
        ])
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run:
            Path("repo").mkdir()
            run.side_effect = [
                completed(["git", "remote", "-v", "update"]),
                completed(["git", "status"], stdout=status),
            ]

            outdated = versioning.GITRepo("repo", "git@example.test:groupe/projet.git").outdated()

        self.assertTrue(outdated)
        self.assertEqual(run.call_count, 2)

    def test_outdated_raises_explicit_error_without_upstream(self):
        status = "\n".join([
            "# branch.oid abc123",
            "# branch.head master",
        ])
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run:
            Path("repo").mkdir()
            run.side_effect = [
                completed(["git", "remote", "-v", "update"]),
                completed(["git", "status"], stdout=status),
            ]

            with contextlib.redirect_stderr(io.StringIO()), self.assertRaises(versioning.GitCommandError) as ctx:
                versioning.GITRepo("repo", "git@example.test:groupe/projet.git").outdated()

        self.assertIn("No upstream branch is configured", str(ctx.exception))
        self.assertEqual(run.call_count, 2)

    def test_outdated_missing_directory_is_outdated_without_contacting_remote(self):
        with tempfile.TemporaryDirectory() as tmp, temporary_cwd(tmp), mock.patch("pytools.versioning.subprocess.run") as run:
            outdated = versioning.GITRepo("repo", "git@example.test:groupe/projet.git").outdated()

        self.assertTrue(outdated)
        run.assert_not_called()


class CheckSshAccessTests(unittest.TestCase):
    def test_check_ssh_access_accepts_welcome_with_return_code_one(self):
        with mock.patch("pytools.versioning.subprocess.run") as run:
            run.return_value = completed(["ssh"], returncode=1, stdout="Welcome to Git server, @user!\n")

            result = versioning.check_ssh_access("example.test")

        self.assertEqual(result.returncode, 1)
        cmd = run.call_args.args[0]
        self.assertIn("ssh", cmd)
        self.assertIn("ConnectionAttempts=1", cmd)
        self.assertIn("-T", cmd)
        self.assertIn("git@example.test", cmd)


if __name__ == "__main__":
    unittest.main()
