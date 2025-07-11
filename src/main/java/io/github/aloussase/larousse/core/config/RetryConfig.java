package io.github.aloussase.larousse.core.config;

import io.github.resilience4j.core.IntervalFunction;
import io.github.resilience4j.retry.Retry;
import io.github.resilience4j.retry.RetryRegistry;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class RetryConfig {

    @Bean
    public Retry providesRetry() {
        final var config = io.github.resilience4j.retry.RetryConfig.custom()
                .maxAttempts(3)
                .intervalFunction(IntervalFunction.ofExponentialRandomBackoff())
                .build();
        final var registry = RetryRegistry.of(config);
        return registry.retry("searchService", config);
    }

}
