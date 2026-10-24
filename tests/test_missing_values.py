"""Run the CSV/missing-value tests in isolated directories with gfortran."""
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class MissingValues(unittest.TestCase):
    def test_missing_values(self):
        with tempfile.TemporaryDirectory(prefix='dataframe-na-') as tmp:
            directory=Path(tmp)
            for optimization in ('-O0','-O2'):
                for program in ('xdataframe_na','xdataframe_na_edges'):
                    exe=directory/(program+'.exe')
                    built=subprocess.run(['gfortran',optimization,'-fcheck=all',
                        *[str(ROOT/n) for n in ('kind.f90','util.f90','dataframe.f90',program+'.f90')],
                        '-o',str(exe)],cwd=directory,capture_output=True,text=True,timeout=60)
                    self.assertEqual(built.returncode,0,built.stderr)
                    run=subprocess.run([exe],cwd=directory,capture_output=True,text=True,timeout=15)
                    self.assertEqual(run.returncode,0,run.stdout+run.stderr)
                    self.assertIn(program+': PASS',run.stdout)
                    if program.endswith('edges'):
                        for mode,diagnostic in [('axis','axis must'),('how','how must'),
                            ('method','method must'),('both','exactly one'),('neither','exactly one'),
                            ('unallocated','values not allocated')]:
                            run=subprocess.run([exe,mode],cwd=directory,capture_output=True,text=True,timeout=15)
                            self.assertNotEqual(run.returncode,0)
                            self.assertIn(diagnostic,run.stderr)

if __name__=='__main__':
    unittest.main()
