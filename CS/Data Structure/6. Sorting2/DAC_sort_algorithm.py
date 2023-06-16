def merge(items, temp, low, mid, high):
    i = low
    j = mid+1
    for k in range(low, high+1):
        if i > mid:
            temp[k] = items[j]
            j += 1
        elif j > high:
            temp[k] = items[i]
            i += 1
        elif items[j] < items[i]:
            temp[k] = items[j]
            j += 1
        else:
            temp[k] = items[i]
            i += 1
    for k in range(low, high+1):
        items[k] = temp[k]

def merge_sort(items, temp, low, high):
    if high <= low:
        return None
    mid = low + (high-low) // 2
    merge_sort(items, temp, low, mid)
    merge_sort(items, temp, mid+1, high)
    merge(items, temp, low, mid, high)


def partition(items, pivot, high):
    i = pivot + 1
    j = high
    while True:
        while i < high and items[i] < items[pivot]:
            i += 1
        while j > pivot and items[j] > items[pivot]:
            j -=1
        if j <= i:
            break;
        items[i], items[j] = items[j], items[i]
        i += 1
        j -= 1
    items[pivot], items[j] = items[j], items[pivot]
    return j

def quick_sort(items, low, high):
    if low < high:
        pivot = partition(items, low, high)
        quick_sort(items, low, pivot-1)
        quick_sort(items, pivot+1, high)




if __name__ == '__main__':
    items = [54, 88, 77, 26, 93, 17, 49, 10, 17, 77, 11, 31, 22, 44, 17, 20]
    temp = [None] * len(items)
    print('합병 정렬')
    print('정렬 전:\t', end='')
    print(items)
    merge_sort(items, temp, 0, len(items)-1)
    print('정렬 후:\t', end='')
    print(items)

    items2 = [54, 88, 77, 26, 93, 17, 49, 10, 17, 77, 11, 31, 22, 44, 17, 20]
    print('퀵 정렬')
    print('정렬 전:\t', end='')
    print(items2)
    print('정렬 후:\t', end='')
    quick_sort(items2, 0, len(items) - 1)
    print(items2)