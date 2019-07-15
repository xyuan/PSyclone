# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: Joerg Henrichs, Bureau of Meteorology

'''This module tests the various classes in core.access_info.'''

from __future__ import absolute_import
import pytest
from psyclone.core.access_info import AccessInfo, VariableAccessInfo, \
    VariablesAccessInfo
from psyclone.core.access_type import AccessType
from psyclone.psyGen import Node, InternalError


def test_access_info():
    '''Test the AccessInfo class.
    '''
    location = 12
    access_info = AccessInfo(AccessType.READ, location, Node())
    assert access_info.access_type == AccessType.READ
    assert access_info.location == location
    assert access_info.indices is None
    access_info.change_read_to_write()
    assert access_info.access_type == AccessType.WRITE
    with pytest.raises(InternalError) as err:
        access_info.change_read_to_write()
    assert "Trying to change variable to 'WRITE' which does not have "\
        "'READ' access." in str(err)

    access_info.indices = ["i"]
    assert access_info.indices == ["i"]

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node())
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices is None

    access_info = AccessInfo(AccessType.UNKNOWN, location, Node(), ["i", "j"])
    assert access_info.access_type == AccessType.UNKNOWN
    assert access_info.location == location
    assert access_info.indices == ["i", "j"]


# -----------------------------------------------------------------------------
def test_variable_access_info():
    '''Test the VariableAccesInfo class, i.e. the class that manages a list
    of VariableInfo instances for one variable
    '''

    vai = VariableAccessInfo("var_name")
    assert vai.var_name == "var_name"
    assert vai.is_written() is False
    assert vai.is_read() is False
    assert vai.all_accesses == []

    vai.add_access(AccessType.READ, Node(), 1)
    assert vai.is_read()
    vai.change_read_to_write()
    assert not vai.is_read()
    assert vai.is_written()


# -----------------------------------------------------------------------------
def test_variables_access_info():
    '''Test the implementation of VariablesAccessInfo, a class that manages
    a list of variables, each with a list of accesses.
    '''
    var_accesses = VariablesAccessInfo()
    node1 = Node()
    var_accesses.add_access("read", AccessType.READ, node1)
    node2 = Node()
    var_accesses.add_access("written", AccessType.WRITE, node2)
    assert str(var_accesses) == "read: READ, written: WRITE"

    var_accesses.next_location()
    node = Node()
    var_accesses.add_access("written", AccessType.WRITE, node)
    var_accesses.next_location()
    var_accesses.add_access("read_written", AccessType.WRITE, node)
    var_accesses.add_access("read_written", AccessType.READ, node)
    assert str(var_accesses) == "read: READ, read_written: READWRITE, "\
                                "written: WRITE"
    assert set(var_accesses.all_vars) == set(["read", "written",
                                              "read_written"])
    all_accesses = var_accesses["read"].all_accesses
    assert all_accesses[0].node == node1
    written_accesses = var_accesses["written"].all_accesses
    assert written_accesses[0].location == 0
    assert written_accesses[1].location == 1
    # Check that the location pointer is pointing to the next statement:
    assert var_accesses.location == 2

    # Create a new instance, which starts with statement number 999
    var_accesses2 = VariablesAccessInfo(999)
    var_accesses2.add_access("new_var", AccessType.READ, node)
    var_accesses2.add_access("written", AccessType.READ, node)
    new_var_accesses = var_accesses2["new_var"].all_accesses
    assert new_var_accesses[0].location == 999

    # Now merge the new instance with the previous instance:
    var_accesses.merge(var_accesses2)
    assert str(var_accesses) == "new_var: READ, read: READ, " \
                                "read_written: READWRITE, written: READWRITE"

    with pytest.raises(KeyError):
        _ = var_accesses["does_not_exist"]
    with pytest.raises(KeyError):
        var_accesses.is_read("does_not_exist")
    with pytest.raises(KeyError):
        var_accesses.is_written("does_not_exist")
