import unittest
from unittest import mock

import progs_cli.updateoffi as updateoffi


class UpdateOffiCliTests(unittest.TestCase):
    def test_verbose_option_is_propagated_to_repositories(self):
        with mock.patch("progs_cli.updateoffi.update_offi") as update_offi:
            updateoffi.main(["--verbose"])

        repos, opts = update_offi.call_args.args
        self.assertTrue(opts["verbose"])
        self.assertTrue(repos)
        self.assertTrue(all(repo.verbose for repo in repos))

    def test_verbose_option_defaults_to_false(self):
        with mock.patch("progs_cli.updateoffi.update_offi") as update_offi:
            updateoffi.main([])

        repos, opts = update_offi.call_args.args
        self.assertFalse(opts["verbose"])
        self.assertTrue(repos)
        self.assertTrue(all(not repo.verbose for repo in repos))


if __name__ == "__main__":
    unittest.main()
