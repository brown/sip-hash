sip-hash
========

A Common Lisp implementation of SipHash, a cryptographically strong family of
hash functions designed by Jean-Philippe Aumasson and Daniel J. Bernstein.

For more information
====================

http://131002.net/siphash/
https://github.com/veorq/SipHash")

sip-hash API
============

sip-hash:hash-64-2-4 octets k0 k1 &key (start 0) end

    Returns as an (unsigned-byte 64) the 64-bit SipHash-2-4 hash code for
    positions START through END of OCTETS, using the initial state stored in K0
    and K1.

sip-hash:hash-64-4-8 octets k0 k1 &key (start 0) end

    Returns as an (unsigned-byte 64) the 64-bit SipHash-4-8 hash code for
    positions START through END of OCTETS, using the initial state stored in K0
    and K1.

sip-hash:hash-128-2-4 octets k0 k1 &key (start 0) end

    Returns as two (unsigned-byte 64) values the 128-bit SipHash-2-4 hash code
    for positions START through END of OCTETS, using the initial state stored
    in K0 and K1.

sip-hash:hash-128-4-8 octets k0 k1 &key (start 0) end

    Returns as two (unsigned-byte 64) values the 128-bit SipHash-4-8 hash code
    for positions START through END of OCTETS, using the initial state stored
    in K0 and K1.
