#!/usr/bin/env python3

__author__ = "Michael Ciccotosto-Camp"

from typing import TypeVar, Generic


T = TypeVar("T")


class Queue(Generic[T]):
    def __init__(self) -> None:
        self.__list: list[T] = list()

    def dequeue(self) -> T:
        return self.__list.pop(0)

    def enqueue(self, element: T) -> int:
        self.__list.append(element)
        return len(self)

    def front(self) -> T:
        return self.__list[0]

    def __len__(self) -> int:
        return len(self.__list)

    def __str__(self) -> str:
        return str(self.__list)


def main() -> None:
    int_queue: Queue[int] = Queue()

    int_queue.enqueue(4)
    int_queue.enqueue(5)
    int_queue.enqueue(9)

    print(int_queue.dequeue())
    print(len(int_queue))
    print(int_queue.front())


if __name__ == "__main__":
    main()
