import os
from pathlib import Path
import subprocess
import sys
import unittest
from unittest.mock import patch

from rb_ffmpeg import tools


class ToolTests(unittest.TestCase):
    def test_explicit_then_bundled_then_path(self):
        for success_index in (0, 1, 2):
            seen = []

            def check(candidate):
                seen.append(candidate)
                return len(seen) - 1 == success_index

            with patch.object(tools.shutil, 'which', return_value='path-ffmpeg'):
                result = tools.find_tool('ffmpeg', 'custom', Path('bundle'), check)
            self.assertEqual(result, seen[-1])
            self.assertEqual(len(seen), success_index + 1)
            self.assertIn('custom', seen[0])
            if success_index > 0:
                self.assertIn('bundle', seen[1])
            if success_index == 2:
                self.assertEqual(result, 'path-ffmpeg')

    def test_source_install_has_no_bundled_candidate(self):
        with patch.object(sys, 'frozen', False, create=True), \
                patch.object(tools.shutil, 'which', return_value=None):
            self.assertEqual(tools.find_tool('ffmpeg', check=lambda _: self.fail()), '')

    def test_frozen_install_locates_bundled_tools(self):
        with patch.object(sys, 'frozen', True, create=True), \
                patch.object(tools.shutil, 'which', return_value=None):
            result = tools.find_tool('ffmpeg', check=lambda _: True)
        self.assertIn(str(Path('_vendor') / 'ffmpeg' / 'bin'), result)

    def test_missing_or_broken_tool(self):
        self.assertFalse(tools.check_tool('this-program-does-not-exist-rb-ffmpeg'))
        with patch.object(tools.shutil, 'which', return_value='broken'):
            self.assertEqual(tools.find_tool('ffmpeg', check=lambda _: False), '')

    def test_launch_without_standard_streams_and_capture_both_outputs(self):
        with patch.object(sys, 'stdin', None), patch.object(sys, 'stdout', None), \
                patch.object(sys, 'stderr', None):
            with tools.start_tool([sys.executable, '-c',
                    'import sys; print("output"); print("error", file=sys.stderr)']) as process:
                output, _ = process.communicate(timeout=15)
        self.assertEqual(process.returncode, 0)
        self.assertIn('output', output)
        self.assertIn('error', output)


if __name__ == '__main__':
    unittest.main()

