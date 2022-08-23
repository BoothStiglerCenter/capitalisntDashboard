@echo off
echo Starting Automated API Collection

echo ---------------------
echo ------ CONDA --------
echo ---------------------
call activate ioCapture


echo ---------------------
echo ------ PYTHON -------
echo ---------------------
@REM python HelloWorld.py
python apiCollect.py



echo ---------------------
echo ------ ENDING -------
echo ---------------------

