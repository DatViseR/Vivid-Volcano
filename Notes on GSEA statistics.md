# Comments on statistics used for GSEA

## Due to hierarchical nature of GO tags the hypergeometric test pvalue adjustment by FDR and similar may not be fully optimal (hierarchical methods will be implemented in the future), redundancy in result can be filtered by semantic measure.

## In which scenarion the adjusted pvalues may be not optimal:

### BH is Too Conservative (Over-Adjusts)

##### Scenario:

A parent term is truly biologically meaningful, but its significance is
diluted because many dependent child terms (which inherit its genes) are
tested alongside it.

##### Example:

```         
Parent term: "Immune response" (raw p-value = 0.001).

Child terms: "T cell activation" (raw p-value = 0.0001), "B cell activation" (raw p-value = 0.0002), etc.
```

Problem: BH treats all terms as independent, so it adjusts the parent’s
p-value as if it were one of many independent tests. If there are 1,000
terms tested, the parent’s adjusted p-value might become: 0.001 \*
(1,000 / rank) If the parent’s p-value is ranked 10th, its adjusted
p-value becomes 0.1, which is not significant (at α = 0.05).

Outcome: The truly meaningful parent term ("Immune response") is missed
because BH over-penalizes it due to the large number of dependent child
terms tested.

### BH is Too Liberal (Under-Adjusts)

##### Scenario:

A parent term is not biologically meaningful, but its p-value is
artificially inflated because its significance is driven solely by
overlapping genes from its child terms.

##### Example:

```         
Parent term: "Cellular process" (raw p-value = 0.02) – a very broad, non-specific term.

Child terms: "Mitochondrial ATP synthesis" (raw p-value = 0.0001), "Nuclear DNA repair" (raw p-value = 0.0003).
```

Problem: The parent term "Cellular process" is enriched only because it
includes genes from its significant child terms. BH treats the parent
and child terms as independent, so the parent’s adjusted p-value might
remain significant (e.g., 0.02 \* (1,000 / rank)). If the parent’s
p-value is ranked 500th, its adjusted p-value is 0.02 \* (1,000 / 500) =
0.04, which is significant at α = 0.05.

Outcome: A non-informative, overly broad term ("Cellular process") is
falsely reported as significant because BH does not account for its
dependency on child terms. Why This Happens

```         
GO Hierarchy: Parent-child terms share genes (due to the true path rule), creating dependency between tests.

BH Assumption: BH assumes tests are independent or positively correlated. Violations (e.g., hierarchical dependencies in GO) lead to:

    Over-adjustment (too conservative) for terms with many dependent subtests.

    Under-adjustment (too liberal) for terms that "piggyback" on their children’s significance.
```
