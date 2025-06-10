# Contributing to Vivid-Volcano

Thank you for your interest in contributing to Vivid-Volcano! This project is a Shiny application for custom analysis of preprocessed omics data, and I welcome contributions from the community.

## 📧 Contact

For questions, suggestions, or discussions about contributions, please reach out to:
**Email:** datviser@gmail.com

## 🎯 Priority Areas for Contribution

I especially encourage contributions in the following areas related to the limitations of Vivid Volcano:

### Statistical Methods Improvements

Based on our [Notes on GSEA statistics](Notes_on_statistics/Notes%20on%20GSEA%20statistics.md), we have identified key limitations in current GSEA statistical approaches:

- **Hierarchical GO tree structure implementation**: We need methods that account for the hierarchical nature of Gene Ontology terms
- **Better p-value adjustment methods**: Alternatives to FDR/BH that handle dependent tests in GO hierarchies
- **Lightweight solutions**: Implementations that don't rely on heavy Bioconductor packages and maintain smooth application performance

### Specific Technical Challenges

1. **Parent-child term dependency**: Addressing over-conservative adjustments when parent terms are diluted by many dependent child terms
2. **False significance prevention**: Avoiding under-adjustment where broad parent terms appear significant only due to their children's genes
3. **Performance optimization**: Maintaining fast computation while implementing more sophisticated statistical methods

## 🚀 How to Contribute

### Reporting Issues

1. **Check existing issues** first to avoid duplicates
2. **Use descriptive titles** that clearly summarize the problem
3. **Provide detailed information**:
   - Steps to reproduce the issue
   - Expected vs. actual behavior
   - Your environment (R version, browser, OS)
   - Screenshots/screen movies if applicable

### Suggesting Features

1. **Check existing feature requests** in the issues
2. **Describe the use case** and why the feature would be valuable
3. **Consider implementation complexity** and performance impact
4. **For statistical methods**: Provide references or theoretical background

### Code Contributions

#### Before You Start

1. **Open an issue** to discuss your proposed changes
2. **Fork the repository** and create a new branch
3. **Keep changes focused** - one feature or fix per pull request

#### Development Guidelines

1. **Maintain Performance**: The app should remain responsive
   - Avoid heavy dependencies (especially large Bioconductor packages)
   - Test with realistic data sizes
   - Consider computational complexity

2. **Code Quality**:
   - Follow R coding conventions
   - Add comments for complex statistical methods
   - Include error handling and user feedback

3. **Testing**:
   - Test new features thoroughly
   - Verify compatibility with existing functionality
   - Test with different data types and sizes

4. **Documentation**:
   - Update relevant documentation
   - Add examples for new statistical methods
   - Include mathematical notation for statistical approaches

#### Statistical Method Contributions

When contributing statistical improvements:

1. **Provide theoretical justification** for the method
2. **Include references** to relevant literature
3. **Compare performance** with existing methods
4. **Document limitations** and appropriate use cases
5. **Consider edge cases** (small sample sizes, sparse data, etc.)

## 📋 Pull Request Process

1. **Create a descriptive title** summarizing your changes
2. **Reference related issues** using keywords like "Fixes #123"
3. **Provide a detailed description**:
   - What changes were made
   - Why they were necessary
   - How they address the problem
   - Any testing performed

4. **Ensure your code**:
   - Follows the project's coding style
   - Includes appropriate documentation
   - Doesn't break existing functionality
   - Maintains or improves performance


## 📚 Resources

- [Gene Ontology Consortium](http://geneontology.org/)
- [R Shiny Documentation](https://shiny.rstudio.com/)
- [Notes on GSEA Statistics](Notes_on_statistics/Notes%20on%20GSEA%20statistics.md) (in this repository)


## 💡 Ideas and Brainstorming

We encourage open discussion about:

- **Novel statistical approaches**
- **Performance optimization** 
- **User experience improvements**
- **Integration with other omics analysis tools**
- **Visualization enhancements**

Feel free to open issues with the "idea" or "discussion" labels to start conversations about potential improvements.

## 🏷️ Issue Labels

To help organize contributions, please use these labels:

- `bug`: Something isn't working correctly
- `enhancement`: New feature or improvement
- `statistics`: Related to statistical methods or algorithms
- `performance`: Performance optimisation
- `ui/ux`: User interface or experience improvements
- `documentation`: Documentation improvements
- `question`: General questions or help needed
- `idea`: Brainstorming or discussion topics

## 🙏 Recognition

All contributors will be acknowledged in the project. Significant contributions may be highlighted in release notes and documentation.Thank you for helping make Vivid-Volcano better for the omics analysis community!