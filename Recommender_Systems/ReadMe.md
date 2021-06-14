# Recommender Systems

This covers areas of focus on recommender systems using AI/ML technologies.

- Ensure Anaconda environment is setup
- Ensure Python 3.6 or later versions are installed
- Ensure surprise package is installed. Use the following ```conda install -c conda-forge scikit-surprise```
- You can refer to [here](https://sundog-education.com/recsys/) for downloading the content

## Analtomy of Top N Architecture and Alternative predicted rating

![Anatomy_TopN_Architecture](/Recommender_Systems/figures/Anatomy_TopN_Architecture.png)
![Architecture_Alternative](/Recommender_Systems/figures/Architecture_Alternative.png)

### Understanding of Recommender Systems
- Implicit and Explicit Ratings examples
  - Implicit
    - Purchase data
    - Video viewing data
    - Click data
  - Explicit
    - Star reviews
    
- Recommender systems
  - Examples of recommender systems
    - Netflix's home page
    - Google search
    - Amazon's "people who bought also bought.."
    - Pandora
    - Youtube
  - Not examples of recommender systems
    - Online radio stations
    - Wikipedia search

- Examples of "Top N" Recommenders
  - Netflix recommendation widgets (Yes)
  - Google search (No)
  - Amazon's "people who bought also bought.." (Yes)
  
- Components of "Top N" Recommenders
  - Candidate generation (Yes)
  - Filtering (Yes)
  - Candidate shuffling (No)
  - Ranking (Yes)
