import hashlib
import importlib.util
import io
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
from urllib.error import URLError
from zipfile import ZipFile

spec = importlib.util.spec_from_file_location('prepare_ffmpeg',
    Path(__file__).resolve().parents[1] / 'packaging' / 'prepare_ffmpeg.py')
prepare = importlib.util.module_from_spec(spec)
spec.loader.exec_module(prepare)


class DownloadTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.folder = Path(self.temp.name)
        self.data = b'verified archive'
        self.manifest = dict(version='test', url='https://example.invalid/ffmpeg.zip',
                             sha256=hashlib.sha256(self.data).hexdigest())

    def test_download_then_verified_cache_without_network(self):
        with patch.object(prepare, 'urlopen', return_value=io.BytesIO(self.data)) as fetch:
            first = prepare.fetch_archive(self.manifest, self.folder)
            self.assertEqual(first.read_bytes(), self.data)
            second = prepare.fetch_archive(self.manifest, self.folder)
        self.assertEqual(first, second)
        fetch.assert_called_once()

    def test_bad_download_never_becomes_cache(self):
        with patch.object(prepare, 'urlopen', return_value=io.BytesIO(b'wrong')):
            with self.assertRaisesRegex(ValueError, 'SHA-256 mismatch'):
                prepare.fetch_archive(self.manifest, self.folder)
        self.assertEqual(list(self.folder.iterdir()), [])

    def test_corrupt_cache_is_rejected_without_redownloading(self):
        archive = self.folder / f"ffmpeg-test-{self.manifest['sha256'][:12]}.zip"
        archive.write_bytes(b'wrong')
        with patch.object(prepare, 'urlopen') as fetch:
            with self.assertRaisesRegex(ValueError, 'SHA-256 mismatch'):
                prepare.fetch_archive(self.manifest, self.folder)
        fetch.assert_not_called()

    def test_network_failure_removes_partial_download(self):
        with patch.object(prepare, 'urlopen', side_effect=URLError('offline')):
            with self.assertRaises(URLError):
                prepare.fetch_archive(self.manifest, self.folder)
        self.assertEqual(list(self.folder.iterdir()), [])

    def archive(self, missing=None, extra=None):
        path = self.folder / 'test.zip'
        with ZipFile(path, 'w') as archive:
            for name in (*('bin/' + tool for tool in prepare.TOOLS), 'LICENSE', 'README.txt'):
                if name != missing:
                    archive.writestr('ffmpeg-test/' + name, b'test')
            if extra:
                archive.writestr(extra, b'unsafe')
        return path

    def test_missing_tool_stops_extraction(self):
        with self.assertRaisesRegex(ValueError, 'Missing FFmpeg program: ffplay.exe'):
            prepare.extract_payload(self.archive(missing='bin/ffplay.exe'), self.folder / 'out')
        self.assertFalse((self.folder / 'out').exists())

    def test_extract_tools_and_upstream_notices(self):
        output = self.folder / 'out'
        prepare.extract_payload(self.archive(), output)
        self.assertTrue((output / 'bin/ffmpeg.exe').is_file())
        self.assertTrue((output / 'README.txt').is_file())
        self.assertTrue((output / 'LICENSE').is_file())

    def test_reject_zip_traversal(self):
        with self.assertRaisesRegex(ValueError, 'Unsafe archive member'):
            prepare.extract_payload(self.archive(extra='ffmpeg-test/../../outside'), self.folder / 'out')


if __name__ == '__main__':
    unittest.main()

