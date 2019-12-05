package main

type IdPool struct {
	Size int32
	Nums []int32
}

func NewIdPool(size int32) *IdPool {
	idpool := &IdPool{Size: size}
	var i int32
	for i = 1; i <= size; i++ {
		idpool.Nums = append(idpool.Nums, i)
	}
	return idpool
}

func (idpool *IdPool) GetId() int32 {
	if len(idpool.Nums) > 0 {
		idpool.Nums = idpool.Nums[1:]
		return idpool.Nums[0]
	} else {
		idpool.Size += 1
		idpool.Nums = append(idpool.Nums, idpool.Size)
		return idpool.Nums[0]
	}
}

func (idpool *IdPool) PutId(id int32) {
	idpool.Nums = append(idpool.Nums, id)
}
