import unittest
from unittest import mock

import progs_cli.bck_rsync as bck_rsync


class BackupRsyncCliTests(unittest.TestCase):
    def test_main_uses_default_options_when_called_without_arguments(self):
        options = {
            "destdir": "/backup/destination",
            "srcdir": "/backup/source",
            "folders": ["web"],
        }
        with (
            mock.patch.object(bck_rsync, "pars", options),
            mock.patch.object(bck_rsync.os.path, "isdir", return_value=True),
            mock.patch.object(bck_rsync.os, "chdir"),
            mock.patch.object(bck_rsync, "tar_folder", return_value="web.tar.bz2"),
            mock.patch.object(bck_rsync.shutil, "copy2") as copy2,
        ):
            bck_rsync.main()

        copy2.assert_called_once_with(
            "web.tar.bz2", "/backup/destination/web.tar.bz2")


if __name__ == "__main__":
    unittest.main()
