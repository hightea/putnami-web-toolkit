/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.plugin.spring.rpc.server.controller;

import javax.annotation.PostConstruct;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
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

	@Autowired
	private ApplicationContext applicationContext;

	@PostConstruct
	public void afterPropertySet() {
		for (String beanName : applicationContext.getBeanDefinitionNames()) {
			scanBean(applicationContext.getBean(beanName), beanName);
		}
	}

	@RequestMapping(value = "/commandService", method = RequestMethod.POST)
	public void processPostRpc(HttpServletRequest request, HttpServletResponse response) throws Throwable {
		try {
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
		scanBean(bean, beanName);
		return bean;
	}

	private void scanBean(Object bean, String name) {
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
	}

	@Override
	public int getOrder() {
		return Ordered.HIGHEST_PRECEDENCE;
	}
}
