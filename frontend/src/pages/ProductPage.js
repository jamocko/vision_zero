import React from 'react'
import {Embed, Container} from 'semantic-ui-react'
import LightHeader from '../components/LightHeader';
import crashRiskMap from '../images/placeholderHoustonMap.png';

const ProductPage = () => (
    <div>
        <LightHeader/>
        <h1 align="center">High Crash Risk Map </h1>
        <Container>
        <Embed
            icon='right circle arrow'
            placeholder={crashRiskMap}
            url='http://www.myfav.es/jack'
        />
        </Container>
    </div>
);

export default ProductPage