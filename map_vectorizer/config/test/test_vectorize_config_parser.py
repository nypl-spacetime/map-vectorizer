import os

from ..vectorize_config_parser import parse

import pytest

def test_parse():
    expected = {
        'basecolors': [
             [205, 200, 186],
             [194, 175, 170],
             [191, 172, 167],
             [185, 172, 163],
             [180, 185, 163],
             [178, 179, 157],
             [207, 202, 182],
             [197, 193, 157],
             [178, 181, 172]
        ],
        'brightness': -50,
        'contrast': 95,
        'thresholdblack': 160,
        'thresholdwhite': 255,
    }
    with open(os.path.abspath(os.path.join(__file__, '..', '..', 'vectorize_config_default.txt'))) as fp:
        observed = parse(fp)
        assert observed == expected
