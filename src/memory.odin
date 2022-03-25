package nordlox

free_objs :: proc() {
	obj := vm.objects
	for obj != nil {
		next := obj.next
		free_obj(obj)
		obj = next
	}
}