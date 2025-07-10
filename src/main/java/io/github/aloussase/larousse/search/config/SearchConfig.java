package io.github.aloussase.larousse.search.config;

import io.github.aloussase.larousse.search.domain.service.SearchService;
import lombok.Getter;
import lombok.Setter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@Getter
@Setter
public class SearchConfig {

    @Value("${larousse.baseUrl}")
    private String larousseBaseUrl;

    @Value("${macaco.baseUrl}")
    private String macacoBaseUrl;

    @Bean
    public SearchService providesSearchService() {
        return new SearchService();
    }

}
