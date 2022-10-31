# Simulating Clifford Circuits

The [Gottesman-Knill theorem](https://en.wikipedia.org/wiki/Gottesman%E2%80%93Knill_theorem) states that Clifford circuits can be simulated in polynomial time on a classical computer.
It is a cornerstone of quantum computing, because many interesting quantum error correcting codes can be implemented using Clifford circuits.
Recently, I have been contributing to and using [Stim](https://github.com/quantumlib/Stim), a high performance Clifford circuit simulator.
The proof of the Gottesman-Knill theorem is important when implementing a simulator.
This page is my notes on the proof so I don't need to go searching through books and papers to find the details.

### Notation

We are going to focus on an $n$ qubit system.
Our state space is $H = \left( \mathbb{C}^2 \right)^{\otimes n}$.
We define the [Pauli operators](https://en.wikipedia.org/wiki/Pauli_matrices) as follows:
$$
I = \begin{pmatrix} 1 & 0 \\ 0 & 1 \end{pmatrix} \quad
X = \begin{pmatrix} 0 & 1 \\ 1 & 0 \end{pmatrix} \quad
Y = \begin{pmatrix} 0 & -i \\ i & 0 \end{pmatrix} \quad
Z = \begin{pmatrix} 1 & 0 \\ 0 & -1 \end{pmatrix}
$$
The Pauli operators satisfy the following relations:
$$
XZ = - ZX \quad
XZ = -iY
$$
The $n$ qubit **Pauli group** is defined as
$$
P_n = \left\{ \alpha p_1 \otimes \cdots \otimes p_n \; | \; \alpha = 1, -1, i, -i \quad p_j = I,X,Y,Z \right\} \subset U(H)
$$
It has $4 \cdot 2^n \cdot 2^n$ elements. If we fix $\alpha = 1$, then the remaining $2^n \cdot 2^n$ elements are a basis of ${\rm End}(H)$. This will be important later. The $n$ qubit **Clifford group** $C_n$ is defined as the normalizer of $P_n$ inside $U(H)$. We shall write $X_i$ for the element of $P_n$ which is $X$ on the ith factor and the identity on all the others. Similarly for $Y_i$ and $Z_i$. A ***Clifford circut*** is a quantum circuit built using only elements from the Clifford group.

### Structure of the Clifford Group

Before we get into the physics, we need to understand the structure of the Clifford group.
Let ${\rm Aut}_{\mathbb{C}}(P_n)$ be the automorphisms of $P_n$ that intertwine the $\mathbb{C}$-action.
We are explicitly excluding complex conjugation.
There is a natural map $C_n \to {\rm Aut}_{\mathbb{C}}(P_n)$ defined by $c \cdot p = c p c^{-1}$ inside $U(H)$. If $c \in C_n$ is in the kenerl of this map, then it commutes with every element of $P_n$. Since $P_n$ spans ${\rm End}(H)$ and the center of ${\rm End}(H)$ is the diagonal matrices, $c \in S^1 \subseteq U(H)$.

On the otherhand, if $f \in {\rm Aut}_{\mathbb{C}}(P_n)$, then $f$ extends to an automorphism $F : {\rm End}(H) \to {\rm End}(H)$. By the [Skolem-Noether theorem](https://en.wikipedia.org/wiki/Skolem%E2%80%93Noether_theorem), $F$ is equal to conjugation by some invertible element $c \in {\rm GL}(H)$. Choose elements $p_i \in P_n$ so that $c p_i c^{-1} = Z_i$. Every element of $P_n$ is hermitian or skew-hermitian. Since each $p_i$ has real eigenvalues, they must be hermitian. Choose algebraically indipendent real numbers $\lambda^1, \dots, \lambda^n$. Then $\lambda^i p_i$ is hermitian with no repeated eigenvalues, and diagonalized by $c$. Therefore, $c$ must be unitary, so $c \in C_n$.

To summarize, the natural map $C_n \to {\rm Aut}_{\mathbb{C}}(P_n)$ induces an isomorphism $C_n / S^1 \cong {\rm Aut}_{\mathbb{C}}(P_n)$. This isomoprhism is a corner stone of quantum information theory, and is usually treated as an identity. We shall do the same.

### Hadamard and CNOT gates

### Stabilizer states
