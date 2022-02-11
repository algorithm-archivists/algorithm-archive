# Cryptography

Humans have almost always been interested in sending secret messages that only the sender and receiver understand.
The reason for this is obvious: secret messages should remain secret.
The easiest way for this to happen is to talk behind closed doors, but that simply doesn't work if the sender and receiver are separated by a significant distance.
In this case, they need to rely on a messenger or mailman to send the message.

For simplicity, let's assume they are sending a written letter for the purpose of negotiating war tactics in ancient Greece or Rome.
Obviously, the message can remain secret if both the sender and receiver also trust the messenger; however, what if the messenger is actually an evil spy?
What if the messenger is killed and the letter is stolen?
What if (in an elaborate ruse), some third party slips into the messenger's tent in the dead-of-night and replaces the letter with another one entirely different?

These are all important questions cryptography addresses.

The idea is simple: we procedurally scramble the message we are sending and only provide the unscrambling procedure to trusted parties.
In this way, the message would seem like utter gobbledygook to anyone other than the sender and receiver.
It doesn't matter if the messenger is evil.
They cannot read the message anyway.
It's also fine if the message is replaced, because then the receiver won't be able to properly decode the message and can just ask for another message to be sent (probably on another path with a different messenger).
Unsurprisingly, a very early method of encryption was supposedly developed by Julius Caeser and called the "Caesar Cipher" {{ "ceasar_cipher_wiki" | cite }}.
Here, every character in the message is replaced by another character based on some pre-defined table or chart that only the sender and receiver have.
The table is created by simply rotating the alphabet by $$n$$ spaces, where $$n$$ is chosen in a discussion between the sender and receiver before-hand.

 | $$n$$ | 0 | 2 | 14 | 18 | 21 | 24 | 
 | ----- | - | - | -- | -- | -- | -- |
 | a | a | c | o | s | v | y | 
 | b | b | d | p | t | w | z | 
 | c | c | e | q | u | x | a | 
 | d | d | f | r | v | y | b | 
 | e | e | g | s | w | z | c | 
 | f | f | h | t | x | a | d | 
 | g | g | i | u | y | b | e | 
 | h | h | j | v | z | c | f | 
 | i | i | k | w | a | d | g | 
 | j | j | l | x | b | e | h | 
 | k | k | m | y | c | f | i | 
 | l | l | n | z | d | g | j | 
 | m | m | o | a | e | h | k | 
 | n | n | p | b | f | i | l | 
 | o | o | q | c | g | j | m | 
 | p | p | r | d | h | k | n | 
 | q | q | s | e | i | l | o | 
 | r | r | t | f | j | m | p | 
 | s | s | u | g | k | n | q | 
 | t | t | v | h | l | o | r | 
 | u | u | w | i | m | p | s | 
 | v | v | x | j | n | q | t | 
 | w | w | y | k | o | r | u | 
 | x | x | z | l | p | s | v | 
 | y | y | a | m | q | t | w | 
 | z | z | b | n | r | u | x | 

It is certainly not the most complicated scheme out there, but it is generally the first encryption scheme people come up with when trying to encode secret messages to one another.
Honestly, I remember sending messages back and forth to friends in elementary school, but we would never provide the necessary table to decode the message.
Instead, we would provide enough text that they could find the table themselves from context.
If a bunch of elementary school kids can figure out how to break this encryption scheme, it cannot be too robust.
In fact, it's interesting to see how the field of cryptography has grown since the Caesar cipher was developed.
In the cryptographic literature, there is always a sender, receiver, and eavesdropper.
For some reason beyond my own comprehension, the first two people are almost always given the names Alice (sender) and Bob (receiver).
Meanwhile, the attacker or eavesdropper is usually called either Eve or Charlie
These names are consistent even with quantum cryptography, so they are here to stay.

In general, there are two different types of encryption: symmetric and asymmetric.
Both of which are described in the following sections.

Cryptographic systems are a cornerstone to modern information technology and lie at the heart of everything from WiFi to online banking.
If an attacker manages to crack modern cryptographic algorithms, they could cause serious damage.
For this reason, it is important to keep a few things in mind:
* Because cryptography has become such an advanced field cryptographic systems should be analyzed by trained professionals and have undergo extensive testing and vetting.
    This means that whenever possible, one should use a widely accepted cryptography library instead of writing their own cypher.
* Kerckhoffs's principle says that when determining the robustness of a cryptographic system it should be assumed that the attacker knows the encryption and decryption algorithm {{ "Kerckhoffs_principle_wiki" | cite }}.
    This does not include any pre-shared or secret keys.
* With the advances in technology, cryptography often hits its limits.
    Many formerly state-of-the-art hashing algorithms have become obsolete because the computers used to crack them have gotten faster and better.
    Another field that cryptography will have to face is [quantum computing](../quantum_information/quantum_information.md).
    Quantum computers will have a big impact on cryptography and especially asymmetric cryptography.
    This whole set of problems is summarized in the field of post-quantum cryptography {{ "post-quantum_crypto_wiki" | cite }}.

## Symmetric Cryptography

Symmetric cryptography is called symmetric because the key that is used is the same for encrypting and decrypting. 
For this to work Alice and Bob both need the same key, which they have to share before communicating. 
Some examples for symmetric cryptography are:
* **Ceasar Cipher**: Alice and Bob rotate the alphabet by $$n$$ characters and use that as a table to encode and decode their message {{ "ceasar_cipher_wiki" | cite }}.
* **Rot13**: This is a special case of the Caeser Cipher where the alphabet is rotated by 13, hence the name "Rot13" {{ "rot13_wiki" | cite }}
* **Permutation Cipher**: Here we choose a permutation $$\pi$$ (i.e. $$\pi=(3,1,2,4)$$) and reorder the the letters according to that $$\pi$$ which is the key {{ "CC_permutation" | cite }}.
* **XOR encryption**: This method works on bit strings and combines the message and a key of equal bit length with the XOR operator {{ "xor_cipher_wiki" | cite }}.
    To decrypt, simply XOR again with the same key.
* **DES or Data Encryption Standard**: This is a newer encryption algorithm which was standardized in 1977 {{ "DES_wiki" | cite }}. 
    It has since been deemed insecure and is superseded by AES.
* **AES or Advanced Encryption Standard**: The actual algorithm is called "Rijndael" {{ "AES_wiki" | cite }}. 
    Like with XOR or DES we generate a bit string (depending on which AES you use 128/192 or 256 bit long) which is your key.
* **Blowfish**: This algorithm was also a good contender for the AES but lost to Rijndael {{ "blowfish_cipher_wiki" | cite }}.

This section is currently a work-in-progress, and all of these methods will have corresponding chapters in the near future.

## Asymmetric Cryptography

Asymmetric cryptography is sometimes called "public key cryptography" (or PK cryptography in short) because Bob and Alice both need a shared public key and a private key they keep to themselves.
These algorithms are called asymmetric because what is encrypted with the public key can only be decrypted with the private key and vice versa. 
This can be used for a number of different applications, like digital signing, encrypted communication, or secretly sharing keys.
For example, if Alice wants to send a message to Bob and this message has to be kept private, Alice will encrypt the message with Bob's public key.
Now only Bob can decrypt the message again and read it.
If Charlie were to alter Alice's message, Bob couldn't decrypt it anymore.
If Bob wants to make sure the message is actually from Alice, Alice can encrypt the already encrypted message with her private key again.
This is to keep Charlie from sending forged or altered messages since Bob couldn't decrypt that layer with Alice's public key.
Some examples for public key cryptography:
* **RSA**: This algorithm calculates a public and a private key from two very large primes {{ "RSA_wiki" | cite }}. 
    It is (hopefully) near impossible to factor the product of two such primes in a feasible amount of time.
* **ECC or Elliptic-curve cryptography**: Here you calculate the private and public key from two points on an elliptic curve {{ "ECC_crypto_wiki" | cite }}. 
    This has the positive side effect that you need smaller numbers than non-ECC algorithms like RSA to achieve the same level of security.

This section is currently a work-in-progress. These methods will also have corresponding chapters in the near future.

### Bibliography

{% references %} {% endreferences %}

<script>
MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
</script>

## License
The text of this chapter was written by [Liikt](https://github.com/Liikt) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).
The code examples are licensed under the MIT license (found in LICENSE.md).

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [Liikt](https://github.com/Liikt) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)
