class SamplicsError(Exception):
    """Type of errors"""

    pass


class SinglePSUError(SamplicsError):
    """Only one PSU in the stratum"""

    pass


class ProbError(SamplicsError):
    """Not a valid probability"""

    pass


class MethodError(SamplicsError):
    """Method not applicable"""

    pass


class CertaintyError(SamplicsError):
    """Method not applicable"""

    pass


class DimensionError(SamplicsError):
    """Method not applicable"""

    pass
