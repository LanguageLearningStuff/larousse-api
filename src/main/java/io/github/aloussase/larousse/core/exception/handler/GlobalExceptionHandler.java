package io.github.aloussase.larousse.core.exception.handler;

import io.github.aloussase.larousse.core.exception.DomainException;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;

import java.util.HashMap;

@RestControllerAdvice
public class GlobalExceptionHandler {


    @ExceptionHandler(DomainException.class)
    public ResponseEntity<?> handleDomainException(DomainException ex) {
        final var o = new HashMap<String, Object>();
        o.put("error", ex.getMessage());
        return ResponseEntity.badRequest().body(o);
    }

}
