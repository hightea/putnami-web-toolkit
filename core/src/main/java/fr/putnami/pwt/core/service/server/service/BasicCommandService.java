/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.service.server.service;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;

public class BasicCommandService extends AbstractCommandService {

	/**
	 *
	 */
	private static final long serialVersionUID = 9020376780559340667L;
	private static final String PARAM_SERVICES = "services";

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		String servicesParam = config.getInitParameter(BasicCommandService.PARAM_SERVICES);
		if (servicesParam != null) {
			String[] serviceToInstanciate = servicesParam.split(";");
			if (serviceToInstanciate != null && serviceToInstanciate.length > 0) {
				for (String serviceName : serviceToInstanciate) {
					if (serviceName != null && serviceName.length() > 0) {
						try {
							this.getClass();
							Class<?> serviceClass = Class.forName(serviceName);
							Object service = serviceClass.newInstance();
							for (Class<?> serviceInterface : serviceClass.getInterfaces()) {
								this.injectService(serviceInterface, service);
							}
						} catch (ClassNotFoundException e) {
							throw new ServletException("Can not load service class " + serviceName, e);
						} catch (InstantiationException e) {
							throw new ServletException("Can not instantiate service " + serviceName, e);
						} catch (IllegalAccessException e) {
							throw new ServletException("Can not instantiate service " + serviceName, e);
						}
					}
				}
			}
		}
	}
}
