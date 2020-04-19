""" Commonly used utility functions. """

import os
import shutil


def clean_up(parent_directory, *files):
    """
    Removes (cleans up) specified files or directories in a directory.
    :param parent_directory: the parent directory of the files.
    :param files: files or directories to remove.
    """
    for file in files:
        file = os.path.join(parent_directory, file)
        if os.path.isdir(file):
            shutil.rmtree(file)
        else:
            os.remove(file)


def create_dir_if_not_exists(dir):
    """
    Creates a directory if not present.
    :param dir: the directory to create
    """
    os.makedirs(dir, exist_ok=True)
