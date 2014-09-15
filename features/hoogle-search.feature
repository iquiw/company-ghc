Feature: Hoogle search

  Scenario: Hoogle candidates
    When I execute hoogle search candidates
    Then hoogle search candidates are:
    """
    ("abs" "acosh" "all" "all" "and" "appendFile"
     "accept" "AF_ARP" "accumArray")
    """
