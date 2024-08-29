#! /usr/bin/python3
# -*- coding: utf-8 -*-
#
# from https://github.com/Cito/fix-overlay-icons/blob/main/fix-overlay-icons.py
# added: run as admin.

"""Fix the overlay icons in Windows."""

# ------------------------------------------------------------------------------
# ask for admin's privileges
# from https://stackoverflow.com/questions/19672352/how-to-run-python-script-with-elevated-privilege-on-windows
# ------------------------------------------------------------------------------
import ctypes, sys

def is_admin():
    try:
        return ctypes.windll.shell32.IsUserAnAdmin()
    except:
        return False

if not is_admin():
    ctypes.windll.shell32.ShellExecuteW(None, "runas", sys.executable, " ".join(sys.argv), None, 1)
    sys.exit()

# ------------------------------------------------------------------------------

import codecs
import os
from datetime import date
import winreg as reg

# The names of all overlay icons that shall be boosted
# (you may add empty lines or comment out certain names):
BOOST = """
    DropboxExt01
    DropboxExt02
    DropboxExt03
    DropboxExt04
    DropboxExt05
    DropboxExt06
    DropboxExt07
    DropboxExt08
    DropboxExt09
    DropboxExt10
"""

# Write a backup of the current registry if necessary:
DO_BACKUP = True

# Make the changes in the registry if necessary
# (set this to False for a test run):
DO_CHANGE_REGISTRY = True

# Automatically restart the Windows Explorer if necessary:
DO_RESTART_EXPLORER = True

# The name of the registry backup file:
BACKUP_FILENAME = 'overlay-icons-{date}.reg'

# The name of the main registry key:
KEY = (r'HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\CurrentVersion'
       r'\Explorer\ShellIconOverlayIdentifiers')
# All registry keys to be changed:
KEYS = (KEY, KEY.replace(r'\Microsoft', r'\WOW6432Node\Microsoft'))


def open_key(key):
    """Get a handle for the given registry key."""
    return reg.OpenKey(reg.HKEY_LOCAL_MACHINE, key.split('\\', 1)[1])


def get_changes(key, boost):
    """Determine which changes need to be made to the registry key.

    Returns the found names, deletes, renames and backup file lines.
    """
    names = set()
    found = set()
    deletes = []
    renames = []
    backup = []

    with open_key(key) as base:
        i = 0
        while True:
            try:
                name = reg.EnumKey(base, i)
                value = reg.QueryValue(base, name)
            except OSError:
                break
            add_to_backup(backup, key, name, value)
            core = name.strip()
            if core in names:
                deletes.append(name)
            else:
                names.add(core)
                if core in boost:
                    found.add(core)
                    core = ' ' + core
                if core != name:
                    renames.append((name, core))
            i += 1

    return found, deletes, renames, backup


def warn_not_found(boost, found):
    """Print a warning if icons to be boosted are not found."""
    not_found = sorted(boost.difference(found))
    if not_found:
        print("ATTENTION: Icon(s) not found:", ', '.join(not_found))


def add_to_backup(backup, key, name, value):
    """Add a value to the backup under the given key."""
    if not backup:
        backup.extend((f'[{key}]', ''))
    backup.extend((f'[{key}\\{name}]', f'@="{value}"', ''))


def change_registry(changes):
    """Change the given changes to the registry."""

    for key in changes:
        print(key.replace('HKEY_LOCAL_MACHINE', 'HKLM', 1) + ':')
        deletes, renames = changes[key]

        with open_key(key) as base:

            for name in deletes:
                print(f'Delete {name!r}')
                reg.DeleteKey(base, name)

            for old_name, new_name in renames:
                print(f'Rename {old_name!r} to {new_name!r}')
                value = reg.QueryValue(base, old_name)
                reg.CreateKey(base, new_name)
                reg.SetValue(base, new_name, reg.REG_SZ, value)
                reg.DeleteKey(base, old_name)


def write_backup(filename, backup):
    """Write a backup file."""
    if '{date}' in filename:
        filename = filename.format(date=date.today().isoformat())

    print('Creating backup file', filename)

    backup = ['Windows Registry Editor Version 5.00', ''] + backup + ['']
    with codecs.open(filename, 'w', 'utf_16_le') as backup_file:
        backup_file.write('\ufeff' + '\r\n'.join(backup))


def restart_explorer():
    """Restart the Windows explorer."""
    print('Restart Windows Explorer')
    if not os.system('taskkill /F /IM explorer.exe'):
        os.system('start explorer.exe')


def main():
    """Fix the overlay icons in the registry for 32 and 64 bit programs."""

    boost = [name.strip() for name in BOOST.splitlines()]
    boost = [name for name in boost if name and not name.startswith('#')]
    boost_num = len(boost)
    if boost_num > 15:
        print(f"ATTENTION: Your boost list contains {boost_num} names,")
        print("but only the first 15 icon names are used by Windows.")
    boost = set(boost)
    if len(boost) != boost_num:
        print("ATTENTION: Your boost list contains duplicate icon names.")

    changes = {}
    all_found = set()
    full_backup = []

    for key in KEYS:
        found, deletes, renames, backup = get_changes(key, boost)
        all_found.update(found)
        if deletes or renames:
            changes[key] = (deletes, renames)
            full_backup.extend(backup)

    if changes:
        warn_not_found(boost, all_found)
        if DO_BACKUP:
            write_backup(BACKUP_FILENAME, full_backup)
        if DO_CHANGE_REGISTRY:
            change_registry(changes)
        if DO_RESTART_EXPLORER:
            restart_explorer()
    else:
        print('No changes to the registry are necessary.')


if __name__ == '__main__':
    main()
