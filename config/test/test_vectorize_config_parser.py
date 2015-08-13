from ..vectorize_config_parser import parse

import pytest

def test_parse():
    with open(os.path.join(__file__, '..', 'vectorize_config_default.txt')) as fp:
        observed = parse(fp)
        assert observed == {}
