import json
import os
from pathlib import Path
import shutil
import sys
import tempfile
import unittest
from unittest.mock import patch

os.environ.setdefault('QT_QPA_PLATFORM', 'offscreen')
from PyQt5.QtCore import QSettings
from PyQt5.QtWidgets import QApplication, QMessageBox
from rb_ffmpeg.ffmpeg_GUI import Window
from rb_ffmpeg.tools import start_tool

PROJECT = Path(__file__).resolve().parents[1]


class GuiTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.app = QApplication.instance() or QApplication([])
        cls.app.setOrganizationName('rb-ffmpeg-tests')
        cls.app.setApplicationName('isolated')
        cls.settings_dir = tempfile.TemporaryDirectory()
        QSettings.setDefaultFormat(QSettings.IniFormat)
        QSettings.setPath(QSettings.IniFormat, QSettings.UserScope, cls.settings_dir.name)

    @classmethod
    def tearDownClass(cls):
        cls.settings_dir.cleanup()

    def setUp(self):
        QSettings().clear()
        self.original_stdout = sys.stdout
        self.window = Window()

    def tearDown(self):
        self.window.close()
        sys.stdout = self.original_stdout

    def test_icon_is_packaged(self):
        self.assertFalse(self.window.windowIcon().isNull())

    def test_missing_ffmpeg_stops_before_launch(self):
        with patch.object(self.window, 'getExe', return_value=''), \
                patch.object(self.window, 'run_command') as run, \
                patch.object(QMessageBox, 'critical') as error:
            self.window.on_convert_Button_pressed()
        run.assert_not_called()
        error.assert_called_once()

    def test_launch_failure_is_reported(self):
        with patch.object(QMessageBox, 'critical') as error:
            self.assertIsNone(self.window.run_command(['rb-ffmpeg-nonexistent-program']))
        error.assert_called_once()

    def test_command_failure_is_logged_and_reported(self):
        with patch.object(QMessageBox, 'critical') as error:
            result = self.window.run_command([sys.executable, '-c',
                'import sys; print("conversion failed", file=sys.stderr); sys.exit(7)'])
        self.assertEqual(result, 7)
        self.assertIn('conversion failed', self.window.textEdit.toPlainText())
        error.assert_called_once()

    def test_settings_persist(self):
        self.window.input_fps_lineEdit.setText('17')
        self.window.workspace_lineEdit.setText('remember this folder')
        self.window.close()
        self.window = Window()
        self.assertEqual(self.window.input_fps_lineEdit.text(), '17')
        self.assertEqual(self.window.workspace_lineEdit.text(), 'remember this folder')

    @unittest.skipUnless(os.environ.get('RB_FFMPEG_TEST_BIN'),
                         'Set RB_FFMPEG_TEST_BIN to run a real FFmpeg conversion')
    def test_check_convert_probe_paths_with_accents_spaces_and_quote(self):
        with tempfile.TemporaryDirectory(prefix='rb-ffmpeg-') as root:
            work = Path(root) / "épreuve 42 d'images"
            work.mkdir()
            for image in (PROJECT / 'test').glob('anim*.png'):
                shutil.copyfile(image, work / image.name)
            self.window.ffmpegfolder_lineEdit.setText(os.environ['RB_FFMPEG_TEST_BIN'])
            self.window.workspace_lineEdit.setText(str(work))
            self.window.outdir_lineEdit.setText(str(work))
            with patch.object(QMessageBox, 'critical') as error:
                self.window.on_check_Button_pressed()
                self.window.on_convert_Button_pressed()
                self.window.on_probe_Button_pressed()
            error.assert_not_called()
            self.assertEqual(len(self.window.pix), 31)
            output = work / 'video.mp4'
            self.assertTrue(output.is_file())
            with start_tool([self.window.getExe('ffprobe'), '-v', 'error',
                    '-show_streams', '-of', 'json', str(output)]) as probe:
                report, _ = probe.communicate(timeout=30)
            stream = json.loads(report)['streams'][0]
            self.assertEqual(stream['codec_name'], 'h264')
            self.assertEqual(stream['r_frame_rate'], '25/1')
            self.assertEqual(stream['width'] % 2, 0)
            self.assertEqual(stream['height'] % 2, 0)


if __name__ == '__main__':
    unittest.main()
