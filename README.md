# Customer Segmentation R

This repository contains code and resources for performing customer segmentation using K-Means Clustering in R. Customer segmentation is a crucial technique in marketing that involves dividing a customer base into distinct groups based on shared characteristics. This enables businesses to target specific segments more effectively and tailor their marketing strategies accordingly.

## Features

- **Data Preprocessing:** Methods for cleaning and preparing customer data for analysis.
- **Segmentation Techniques:** Implementation of the K-means clustering algorithm.
- **Visualization:** Tools for visualizing the segmentation results using ggplot2 and other visualization libraries.
- **Analysis:** Scripts for analyzing the characteristics of each segment to derive actionable insights.
- **Shiny App:** A web application built using Shiny to interactively explore customer segments.
- **Presentation:** A PowerPoint presentation summarizing the findings.

## Dataset

This project uses the [Customer Segmentation dataset](https://www.kaggle.com/datasets/vjchoudhary7/customer-segmentation-tutorial-in-python/data) from Kaggle. The dataset includes various features of customers that can be used for segmentation analysis.

## Getting Started

### Prerequisites

Make sure you have R and RStudio installed on your system. You can download R from [CRAN](https://cran.r-project.org/) and RStudio from [RStudio](https://rstudio.com/products/rstudio/download/).

### Installation

1. Clone the repository:
2. Navigate to the project directory:
3. Install the required packages by running

### Usage

1. **Data Preparation:**
    - Ensure `Mall_Customers.csv` is in the directory.
  
2. **Run Segmentation Script:**
    ```bash
    Rscript customer_segmentation.R
    ```
    - This script will perform data preprocessing, run the K-means algorithm, and save the result in `kmeans_result.rds`.

3. **Shiny App:**
    - To run the Shiny app, open RStudio and set the working directory to the repository directory.
    - Ensure `ui.R` and `server.R` are in the root directory.
    - Run the following command in the R console:
    ```r
    shiny::runApp()
    ```
    - This will start the Shiny app locally, allowing you to interactively explore the customer segments.

### Repository Structure

- `Mall_Customers.csv`: Dataset used for customer segmentation.
- `README.md`: Project overview and instructions.
- `customer_segmentation.R`: Script for data preprocessing and customer segmentation using K-means.
- `kmeans_result.rds`: Saved K-means clustering results.
- `new_customer.pptx`: PowerPoint presentation summarizing the findings and insights.
- `server.R`: Server logic for the Shiny app.
- `ui.R`: User interface for the Shiny app.

## Contributing

We welcome contributions to enhance this project. To contribute:

1. Fork the repository.
2. Create a new branch (`git checkout -b feature/your-feature`).
3. Commit your changes (`git commit -am 'Add new feature'`).
4. Push to the branch (`git push origin feature/your-feature`).
5. Create a new Pull Request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

If you have any questions or feedback, please feel free to open an issue or contact us at [ajiboderafiat@yahoo.com].
