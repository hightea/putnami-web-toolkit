package fr.putnami.pwt.plugin.spring.rpc.server.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import fr.putnami.pwt.core.service.server.service.AbstractCommandService;
import fr.putnami.pwt.plugin.spring.rpc.server.util.RequestThreadLocalUtils;

@Controller
public class CommandServiceController extends AbstractCommandService implements BeanPostProcessor, Ordered {


	private static final long serialVersionUID = 4383424486613678203L;

	@RequestMapping(value = "/commandService", method = RequestMethod.POST)
	public void processPostRpc(HttpServletRequest request, HttpServletResponse response) throws Throwable {
		try {
			RequestThreadLocalUtils.initContext(request, response);
			processPost(request, response);
		}
		finally {
			RequestThreadLocalUtils.resetContext();
		}
	}

	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
		Class<?> implClass = bean.getClass();
		if (AopUtils.isAopProxy(bean)) {
			implClass = AopUtils.getTargetClass(bean);
		}
		Service serviceAnnotation = AnnotationUtils.findAnnotation(implClass, Service.class);
		if (serviceAnnotation != null) {
			for (Class inter : implClass.getInterfaces()) {
				injectService(inter, bean);
			}
		}
		return bean;
	}

	@Override
	public int getOrder() {
		return Ordered.HIGHEST_PRECEDENCE;
	}
}
