#!/usr/bin/env python3

__author__ = "Michael Ciccotosto-Camp"

from typing import TypeVar, Generic


T = TypeVar("T")


class Stack(Generic[T]):
    def __init__(self) -> None:
        self.__list: list[T] = []

    def pop(self) -> T:
        return self.__list.pop()

    def push(self, element: T) -> int:
        self.__list.append(element)
        return len(self)

    def top(self) -> T:
        return self.__list[-1]

    def __len__(self) -> int:
        return len(self.__list)

    def __str__(self) -> str:
        return str(self.__list)


def main() -> None:
    int_stack: Stack[int] = Stack()

    int_stack.push(4)
    int_stack.push(5)
    int_stack.push(9)

    print(int_stack.pop())
    print(len(int_stack))
    print(int_stack.top())


if __name__ == "__main__":
    main()
