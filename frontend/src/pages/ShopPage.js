import React, { useState, useEffect } from 'react';
import { beerService } from '../services/beerService';
import BeerCard from '../components/BeerCard';
import { toast } from 'react-toastify';

function ShopPage() {
  const [beers, setBeers] = useState([]);
  const [categories, setCategories] = useState([]);
  const [loading, setLoading] = useState(true);
  const [filters, setFilters] = useState({
    category: '',
    sort: 'name',
    minPrice: '',
    maxPrice: '',
    search: ''
  });

  useEffect(() => {
    fetchData();
  }, []);

  useEffect(() => {
    fetchBeers();
  }, [filters]);

  const fetchData = async () => {
    try {
      const [beersData, categoriesData] = await Promise.all([
        beerService.getAllBeers(),
        beerService.getCategories()
      ]);
      setBeers(beersData);
      setCategories(categoriesData);
    } catch (error) {
      toast.error('Failed to load data');
      console.error('Fetch error:', error);
    } finally {
      setLoading(false);
    }
  };

  const fetchBeers = async () => {
    try {
      const params = {};
      if (filters.category) params.category = filters.category;
      if (filters.sort) params.sort = filters.sort;
      if (filters.minPrice) params.minPrice = filters.minPrice;
      if (filters.maxPrice) params.maxPrice = filters.maxPrice;

      const data = await beerService.getAllBeers(params);
      
      // Client-side search filter
      let filteredBeers = data;
      if (filters.search) {
        const searchTerm = filters.search.toLowerCase();
        filteredBeers = data.filter(beer =>
          beer.name.toLowerCase().includes(searchTerm) ||
          beer.brewery.toLowerCase().includes(searchTerm)
        );
      }
      
      setBeers(filteredBeers);
    } catch (error) {
      toast.error('Failed to filter beers');
    }
  };

  const handleFilterChange = (key, value) => {
    setFilters(prev => ({ ...prev, [key]: value }));
  };

  const clearFilters = () => {
    setFilters({
      category: '',
      sort: 'name',
      minPrice: '',
      maxPrice: '',
      search: ''
    });
  };

  if (loading) {
    return <div className="loading">Loading beers... 🍺</div>;
  }

  return (
    <div className="shop-page">
      <div className="shop-header">
        <h1>🍺 Our Beer Collection</h1>
        <p>Found {beers.length} amazing beers</p>
      </div>

      <div className="shop-filters">
        <input
          type="text"
          placeholder="Search beers..."
          value={filters.search}
          onChange={(e) => handleFilterChange('search', e.target.value)}
          className="search-input"
        />

        <select
          value={filters.category}
          onChange={(e) => handleFilterChange('category', e.target.value)}
        >
          <option value="">All Categories</option>
          {categories.map(cat => (
            <option key={cat.id} value={cat.id}>{cat.name}</option>
          ))}
        </select>

        <select
          value={filters.sort}
          onChange={(e) => handleFilterChange('sort', e.target.value)}
        >
          <option value="name">Sort by Name</option>
          <option value="price">Price: Low to High</option>
          <option value="price_desc">Price: High to Low</option>
          <option value="alcohol">Alcohol Content</option>
        </select>

        <input
          type="number"
          placeholder="Min Price"
          value={filters.minPrice}
          onChange={(e) => handleFilterChange('minPrice', e.target.value)}
          className="price-input"
        />

        <input
          type="number"
          placeholder="Max Price"
          value={filters.maxPrice}
          onChange={(e) => handleFilterChange('maxPrice', e.target.value)}
          className="price-input"
        />

        <button onClick={clearFilters} className="btn btn-secondary btn-small">
          Clear Filters
        </button>
      </div>

      {beers.length === 0 ? (
        <div className="text-center">
          <h3>No beers found 😔</h3>
          <p>Try adjusting your filters</p>
        </div>
      ) : (
        <div className="beer-grid">
          {beers.map(beer => (
            <BeerCard key={beer.id} beer={beer} />
          ))}
        </div>
      )}
    </div>
  );
}

export default ShopPage;
