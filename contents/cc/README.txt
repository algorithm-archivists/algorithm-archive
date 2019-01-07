The file `license.txt` is an example license with attribution to James Schloss (Leios).
Please modify it with the appropriate attribution before appending to the end of each chapter.

To append this license to the end of all files, execute the following command in the `contents/` directory:
    find . -name "*.md" -print | xargs sed -i '$r cc/license.txt'
