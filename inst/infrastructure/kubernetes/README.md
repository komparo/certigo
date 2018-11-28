# run registry

Create registry and service
```
kubectl delete -f inst/infrastructure/kubernetes/kube-registry.yaml
kubectl create -f inst/infrastructure/kubernetes/kube-registry.yaml
```

# forward port
```
kubectl port-forward --namespace kube-system \
$(kubectl get po -n kube-system | grep kube-registry-v0 | \
awk '{print $1;}') 5000:5000

```
